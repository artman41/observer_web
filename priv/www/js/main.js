

function boundedArray(limit, init) {
    if(!(limit !== undefined && typeof(limit) === "number" && limit > 0))
        throw "limit is invalid!";
    this.limit = limit;
    let data = (init !== undefined && init instanceof Array) ? init : [];
    limitData = (function() {
        if(data.length < this.limit)
            return;
        let start = data.length-this.limit;
        data = data.slice(start < 0 ? 0 : start, data.length);
    }).bind(this);
    this.push = (function(elem) {
        data.push(elem);
        limitData();
    }).bind(this);
    this.pop = (function() {
        limitData();
        return data.shift();
    }).bind(this);
    this.data = (function() {
        return data.slice();
    }).bind(this);
}

function formatBytes(bytes) {
    if(!(bytes !== undefined && typeof(bytes) === "number" && bytes >= 0))
        throw "bytes is invalid!";
    const CHARS = ["B", "KB", "MB", "GB", "TB", "PB"];
    let char_int = 0;
    let _bytes = bytes;
    while(_bytes > 1000) {
        if(char_int > CHARS.length)
            throw "Byte Value is too large!"
        char_int++;
        _bytes /= 1000;
    }
    return _bytes + CHARS[char_int];
}

function setupObserver(navSelector, tabContentSelector) {

    const nav = document.querySelector(navSelector);
    const tabContent = document.querySelector(tabContentSelector);

    if(nav === null)
        throw "invalid nav QuerySelector"
    if(tabContent === null)
        throw "invalid tabContent QuerySelector"
    
    function toDOMElement(html) {
        let template = document.createElement("template");
        template.innerHTML = html;
        return template.content.firstChild;
    }

    function websocket() {
        let websocket = this.websocket;
        function reconnect() {
            let onMessage = undefined;
            if(websocket !== undefined)
                onMessage = websocket.onmessage;
            websocket = new WebSocket("ws://" + window.location.host + "/observer/ws");
            websocket.onclose = function(evt) {
                console.warn("Websocket closed! %O\nReconnecting...", evt);
                reconnect();
            };
            websocket.onerror = function(evt) {
                console.warn("Websocket errored! %O\nReconnecting...", evt);
                reconnect();
            };
            if(onMessage !== undefined)
                websocket.onmessage = onMessage.bind(websocket);
        }
        if(websocket === undefined)
            reconnect();
        return websocket;
    }

    function findOrCreate(id, creationString, parent) {
        let elem = document.getElementById(id);
        if(elem === null) {
            elem = toDOMElement(creationString);
            elem.id = id;
        }
        let owner = parent !== undefined ? parent : document.body;
        if(document.getElementById(id) === null)
            owner.append(elem);
        return elem;
    }

    let loadStats = new boundedArray(100);
    loadStats.startTime = undefined;
    loadStats.currentSecond = -1;

    let charts = {
        Scheduler: null,
        Memory: null,
        IO: null,
    }

    window.charts = charts;

    function createChart(containerId, parentElem, title) {
        let chart = new CanvasJS.Chart(
            findOrCreate(containerId, `<div class="container-fluid" style="height: 25em;"></div>`, parentElem).id, 
            {
                title:{
                    text: title,
                    fontFamily: "Segoe UI",
                    horizontalAlign: "left",
                    fontSize: 12,
                    fontWeight: "bold",
                    // fontColor: "darkgrey",
                },
                data: [
                    {type: "line", dataPoints: []}
                ], 
                axisX: {
                    reversed: true
                },  
                zoomEnabled: true,
                toolTip: {
                    shared: true  
                },
                legend: {
                    verticalAlign: "bottom",
                    horizontalAlign: "center",
                    fontSize: 14,
                    fontWeight: "bold",
                    fontFamily: "calibri",
                    fontColor: "dimGrey",
                    cursor:"pointer",
                    itemclick : function(e) {
                        if (typeof(e.dataSeries.visible) === "undefined" || e.dataSeries.visible) {
                            e.dataSeries.visible = false;
                        }
                        else {
                            e.dataSeries.visible = true;
                        }
                        chart.render();
                    }
                }
            }
        );
    
        /* Cheeky hack to prevent the 'CanvasJS Trial' text from appearing */
        let addEl = chart.addEventListener;
        chart.addEventListener = function(...args) {
            if(args[0] === "dataAnimationIterationEnd")
                return undefined;
            return addEl.apply(this, args);
        }
        
        /* Cheeky hack to prevent the 'CanvasJS.com' text from appearing */
        let f = chart.render;
        function render(...args) {
            f.apply(chart, args);
            chart._creditLink.remove()
        }
        chart.render = render.bind(chart);
        chart.render();
        setInterval(() => chart.render(), 250);
        return chart;
    }

    const handlers = {
        "System": function(tabName, _tabNav, tabPane, tabData) {
            let headers = Object.entries(tabData);
            for(let i=0; i<headers.length; i++) {
                let rowIndex = Math.floor(i / 3);
                let row = findOrCreate(
                    `row-${tabName}-${rowIndex}`, 
                    `<div class="row"></div>`,
                    tabPane
                );
                let [headerName, headerData] = headers[i];
                let headerBlock = findOrCreate(
                    `col-${tabName}-${headerName}`,
                    `<div class="col"><h2>${headerName}</h2><div name="${headerName}-data"></div></div>`,
                    row
                );
                let dataDiv = headerBlock.querySelector(`[name="${headerName}-data"]`);
                let dataSets = Object.entries(headerData);
                for(let i=0; i<dataSets.length; i++) {
                    let [datasetName, dataset] = dataSets[i];
                    let datasetDiv = findOrCreate(
                        `dataset-${tabName}-${datasetName}`,
                        `<div><b></b></div>`,
                        dataDiv
                    );
                    
                    let table = findOrCreate(
                        `table-${tabName}-${datasetName}`,
                        `<table class="table table-sm table-bordered table-hover">
                            <thead>
                                <tr><th colspan="2">${datasetName}</th></tr>
                            </thead>
                            <tbody></tbody>
                        </table>`,
                        datasetDiv
                    )
                    Object.entries(dataset).forEach(([key, value]) => {
                        let tableRow = findOrCreate(
                            `table-row-${tabName}-${key}`,
                            `<tr><td>${key}</td><td></td></tr>`,
                            table.tBodies[0]
                        )
                        tableRow.cells[1].innerText = value;
                    });
                }
            }
        },
        "Load Charts": function(tabName, _tabNav, tabPane, tabData) {
            if(loadStats.startTime === undefined)
                loadStats.startTime = tabData.time_us;
            let second = Math.floor((tabData.time_us - loadStats.startTime)/1000000);
            if(second > loadStats.currentSecond){
                loadStats.push(tabData);
                loadStats.currentSecond++;
            }
            let topHalf = findOrCreate(
                `row-${tabName}-top`,
                `<div class="row" style="padding-right: 1em;"></div>`,
                tabPane
            );

            let schedulerCol = findOrCreate(
                `col-${tabName}-scheduler`,
                `<div class="col"></div>`,
                topHalf
            );

            let data = loadStats.data();

            if(charts.Scheduler === null){
                charts.Scheduler = createChart(`container-${tabName}-scheduler`, schedulerCol, "Scheduler Utilisation (%)");
                charts.Scheduler.options.axisX = {
                    gridThickness: 1,
                    intervalType: "second",
                    labelFormatter: function (e) {
                        return Math.floor((e.value.getTime() - data[0].time_us/1000)/1000) + "s";
                    },
                    reversed: true,
                };
            }

            let schedulerPoints = {};

            for(let i=0; i<data.length; i++) {
                let elem = data[i];
                let entries = Object.entries(elem.scheduler_wall_time);
                for(let i=0; i<entries.length; i++) {
                    let [key, pct] = entries[i];
                    if(schedulerPoints[key] === undefined)
                        schedulerPoints[key] = [];
                    
                    schedulerPoints[key].push({x: elem.time_us/1000, y: pct});
                }
            }

            charts.Scheduler.options.data = [];
            let schedulerKeys = Object.keys(schedulerPoints);
            for(let i=0; i<schedulerKeys.length; i++) {
                let key = schedulerKeys[i];
                charts.Scheduler.options.data.push({
                    type: "line",
                    // indexLabel: "#percent%",
                    percentFormatString: "#0.##",
                    dataPoints: schedulerPoints[key],
                    xValueType: "dateTime",
                    name: `scheduler ${key}`,
                    showInLegend: true
                })
            }

            let bottomHalf = findOrCreate(
                `row-${tabName}-bottom`,
                `<div class="row" style="padding-right: 1em;"></div>`,
                tabPane
            );

            let memoryCol = findOrCreate(
                `col-${tabName}-memory`,
                `<div class="col"></div>`,
                bottomHalf
            );
                
            if(charts.Memory === null) {
                charts.Memory = createChart(`container-${tabName}-memory`, memoryCol, "Memory Usage (MB)");
                charts.Memory.options.axisX = {
                    gridThickness: 1,
                    intervalType: "second",
                    labelFormatter: function (e) {
                        return Math.floor((e.value.getTime() - data[0].time_us/1000)/1000) + "s";
                    },
                    reversed: true,
                };
            }

            let memoryPoints = {};

            for(let i=0; i<data.length; i++) {
                let elem = data[i];
                let entries = Object.entries(elem.memory);
                for(let i=0; i<entries.length; i++) {
                    let [key, bytes] = entries[i];
                    if(memoryPoints[key] === undefined)
                        memoryPoints[key] = [];
                    
                    memoryPoints[key].push({x: elem.time_us/1000, y: bytes/1000000});
                }
            }

            charts.Memory.options.data = [];
            let memoryKeys = Object.keys(memoryPoints);
            for(let i=0; i<memoryKeys.length; i++) {
                let key = memoryKeys[i];
                charts.Memory.options.data.push({
                    type: "line",   
                    dataPoints: memoryPoints[key],
                    xValueType: "dateTime",
                    name: key,
                    showInLegend: true,
                })
            }

            let ioCol = findOrCreate(
                `col-${tabName}-io`,
                `<div class="col"></div>`,
                bottomHalf
            );

            if(charts.IO === null) {
                charts.IO = createChart(`container-${tabName}-io`, ioCol, "IO Usage (B)")
                charts.IO.options.axisX = {
                    gridThickness: 1,
                    intervalType: "second",
                    labelFormatter: function (e) {
                        return Math.floor((e.value.getTime() - data[0].time_us/1000)/1000) + "s";
                    },
                    reversed: true,
                }
            }

            let ioPoints = {
                input: [],
                output: []
            };

            for(let i=0; i<data.length; i++) {
                let elem = data[i];
                ioPoints.input.push({x: elem.time_us/1000, y: elem.io.input});
                ioPoints.output.push({x: elem.time_us/1000, y: elem.io.output});
            }

            charts.IO.options.data = [];
            let ioKeys = Object.keys(ioPoints);
            for(let i=0; i<ioKeys.length; i++) {
                let key = ioKeys[i];
                charts.IO.options.data.push({
                    type: "line",   
                    dataPoints: ioPoints[key],
                    xValueType: "dateTime",
                    name: key,
                    showInLegend: true,
                })
            }
        }
    };

    websocket().onmessage = function(event) {
        let object = JSON.parse(event.data);
        let tabs = Object.entries(object);
        for(let i=0; i<tabs.length; i++) {
            let [tabName, tabData] = tabs[i];
            
            let handler = handlers[tabName];
            if(handler === undefined) {
                // console.error("No handler for %O!", tabName);
                continue;
            }

            let fixedTabName = tabName.replace(" ", "_", -1);
            let tabNav = 
                findOrCreate(
                    `nav-item-${fixedTabName}`,
                    `<li class="nav-item" role="presentation">
                        <button class="nav-link ${i == 0 ? "active" : ""}" data-bs-toggle="tab" data-bs-target="#tab-pane-${fixedTabName}" type="button" role="tab" aria-controls="tabContent-${fixedTabName}" aria-selected="true">${tabName}</button>
                    </li>`,
                    nav
                )
            let tabPane = findOrCreate(
                `tab-pane-${fixedTabName}`, 
                `<div class="tab-pane fade container-fluid ${i == 0 ? "show active" : ""}" role="tabpanel"></div>`,
                tabContent
            );
            if(tabData.type === "exception") {
                console.warn("%O failed with error {%O, %O}", tabName, tabData.class, tabData.reason);
                return;
            }
            delete tabData.type;
            handler(fixedTabName, tabNav, tabPane, tabData);
        };
    }

    // let tabContent = toDOMElement(`<div class="tab-pane fade show active" id="tabEmpty" role="tabpanel"></div>`)

}