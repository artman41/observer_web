

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
            findOrCreate(containerId, `<div style="padding-bottom: 50%" class="container-fluid"></div>`, parentElem).id, 
            {
                title:{text: title},
                data: [
                    {type: "line", dataPoints: []}
                ], 
                axisX: {
                    reversed: true
                },
            }
        );
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
            /*
                {
                    "time_us": <number>,
                    "io": {
                        "input": <number>,
                        "output": <number>
                    },
                }
            */
            let topHalf = findOrCreate(
                `row-${tabName}-top`,
                `<div class="row"></div>`,
                tabPane
            );

            let schedulerCol = findOrCreate(
                `col-${tabName}-scheduler`,
                `<div class="col"></div>`,
                topHalf
            );

            let data = loadStats.data();

            if(charts.Scheduler === null)
                charts.Scheduler = createChart(`container-${tabName}-scheduler`, schedulerCol, "Scheduler Utilisation (%)")
            // TODO:

            let bottomHalf = findOrCreate(
                `row-${tabName}-bottom`,
                `<div class="row"></div>`,
                tabPane
            );

            let memoryCol = findOrCreate(
                `col-${tabName}-memory`,
                `<div class="col"></div>`,
                bottomHalf
            );
                
            if(charts.Memory === null)
                charts.Memory = createChart(`container-${tabName}-memory`, memoryCol, "Memory Usage (MB)")

            let memoryPoints = {};

            for(let i=0; i<data.length; i++) {
                let elem = data[i];
                let entries = Object.entries(elem.memory);
                for(let i=0; i<entries.length; i++) {
                    let [key, bytes] = entries[i];
                    if(memoryPoints[key] === undefined)
                        memoryPoints[key] = [];
                    
                    memoryPoints[key].push({x: elem.time_us/1000, y: bytes/1000000, label: `${key}`});
                }
            }

            let tmpData = [];
            let memoryKeys = Object.keys(memoryPoints);
            for(let i=0; i<memoryKeys.length; i++) {
                let key = memoryKeys[i];
                tmpData.push({
                    type: "line",   
                    dataPoints: memoryPoints[key],
                    xValueType: "dateTime",
                    label: key
                })
            }
            charts.Memory.options.axisX = {
                gridThickness: 1,
                intervalType: "second",
                labelFormatter: function (e) {
                    return Math.floor((e.value.getTime() - data[0].time_us/1000)/1000) + "s";
                },
                reversed: true,
              },
            charts.Memory.options.data = tmpData;

            let ioCol = findOrCreate(
                `col-${tabName}-io`,
                `<div class="col"></div>`,
                bottomHalf
            );

            if(charts.IO === null)
                charts.IO = createChart(`container-${tabName}-io`, ioCol, "IO Usage (B)")

            let ioPoints = {
                input: [],
                output: []
            };

            for(let i=0; i<data.length; i++) {
                let elem = data[i];
                ioPoints.input.push({x: elem.time_us/1000, y: elem.io.input, label: "input"});
                ioPoints.output.push({x: elem.time_us/1000, y: elem.io.output, label: "output"});
            }

            tmpData = [];
            let ioKeys = Object.keys(ioPoints);
            for(let i=0; i<ioKeys.length; i++) {
                let key = ioKeys[i];
                tmpData.push({
                    type: "line",   
                    dataPoints: ioPoints[key],
                    xValueType: "dateTime",
                    label: key
                })
            }
            charts.IO.options.axisX = {
                gridThickness: 1,
                intervalType: "second",
                labelFormatter: function (e) {
                    return Math.floor((e.value.getTime() - data[0].time_us/1000)/1000) + "s";
                },
                reversed: true,
            },
            charts.IO.options.data = tmpData;
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