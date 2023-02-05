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

    websocket().onmessage = function(event) {
        let object = JSON.parse(event.data);
        let tabs = Object.entries(object);
        for(let i=0; i<tabs.length; i++) {
            let [tabName, tabData] = tabs[i];
            let fixedTabName = tabName.replace(" ", "_", -1);
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
                continue;
            }
            delete tabData.type;
            let headers = Object.entries(tabData);
            for(let i=0; i<headers.length; i++) {
                let rowIndex = Math.floor(i / 3);
                let row = findOrCreate(
                    `row-${fixedTabName}-${rowIndex}`, 
                    `<div class="row"></div>`,
                    tabPane
                );
                let [headerName, headerData] = headers[i];
                let headerBlock = findOrCreate(
                    `col-${fixedTabName}-${headerName}`,
                    `<div class="col"><h2>${headerName}</h2><div name="${headerName}-data"></div></div>`,
                    row
                );
                let dataDiv = headerBlock.querySelector(`[name="${headerName}-data"]`);
                let dataSets = Object.entries(headerData);
                for(let i=0; i<dataSets.length; i++) {
                    let [datasetName, dataset] = dataSets[i];
                    let datasetDiv = findOrCreate(
                        `dataset-${fixedTabName}-${datasetName}`,
                        `<div><b></b></div>`,
                        dataDiv
                    );
                    
                    let table = findOrCreate(
                        `table-${fixedTabName}-${datasetName}`,
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
                            `table-row-${fixedTabName}-${key}`,
                            `<tr><td>${key}</td><td></td></tr>`,
                            table.tBodies[0]
                        )
                        tableRow.cells[1].innerText = value;
                    });
                }
            }
        }
    }

    // let tabContent = toDOMElement(`<div class="tab-pane fade show active" id="tabEmpty" role="tabpanel"></div>`)

}