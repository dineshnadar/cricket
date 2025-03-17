
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Metadata Form Builder with References</title>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.1/themes/base/jquery-ui.min.css">
    <style>
        body {
            font-family: Arial, sans-serif;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .container {
            display: flex;
            gap: 20px;
        }
        .catalog-panel {
            width: 300px;
            display: flex;
            flex-direction: column;
            gap: 15px;
        }
        .catalog {
            background: white;
            border-radius: 5px;
            padding: 15px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        .catalog-header {
            font-weight: bold;
            margin-bottom: 10px;
            padding-bottom: 5px;
            border-bottom: 1px solid #eee;
            display: flex;
            justify-content: space-between;
        }
        .add-new {
            cursor: pointer;
            color: #4CAF50;
        }
        .section-catalog-item, .field-catalog-item {
            padding: 8px 12px;
            margin-bottom: 8px;
            cursor: move;
            border-radius: 3px;
            position: relative;
        }
        .section-catalog-item {
            background: #e3f2fd;
            border: 1px solid #bbdefb;
        }
        .field-catalog-item {
            background: #e8f5e9;
            border: 1px solid #c8e6c9;
        }
        .catalog-item-id {
            font-size: 0.7em;
            color: #666;
            display: block;
            margin-top: 3px;
        }
        .form-container {
            flex: 1;
            background: white;
            border-radius: 5px;
            padding: 15px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            min-height: 500px;
        }
        .section {
            background: #f5f5f5;
            border-radius: 5px;
            padding: 10px;
            margin-bottom: 10px;
        }
        .section-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 10px;
            font-weight: bold;
        }
        .section-title {
            display: flex;
            flex-direction: column;
        }
        .section-id {
            font-size: 0.7em;
            color: #666;
            font-weight: normal;
        }
        .nested-section {
            background: #eaf5ff;
            border-radius: 5px;
            padding: 10px;
            margin: 10px 0 10px 20px;
            border: 1px solid #d0e3ff;
        }
        .section-drop-area, .nested-section-drop-area {
            min-height: 50px;
            border: 1px dashed #ccc;
            border-radius: 3px;
            margin-top: 10px;
            padding: 10px;
        }
        .field-item {
            background: #e8f5e9;
            border: 1px solid #c8e6c9;
            border-radius: 3px;
            padding: 8px;
            margin: 5px 0;
            position: relative;
        }
        .field-id {
            font-size: 0.7em;
            color: #666;
            display: block;
            margin-top: 3px;
        }
        .dropzone-active {
            background: #e8f5e9;
            border: 2px dashed #66bb6a;
        }
        .buttons {
            margin-top: 10px;
        }
        button {
            background: #4CAF50;
            color: white;
            border: none;
            padding: 5px 10px;
            border-radius: 3px;
            cursor: pointer;
            margin-right: 5px;
        }
        button:hover {
            background: #43a047;
        }
        .empty-message {
            color: #757575;
            font-style: italic;
            padding: 10px;
            text-align: center;
        }
        .field-type {
            font-size: 0.8em;
            color: #757575;
            margin-left: 5px;
        }
        .item-actions {
            float: right;
        }
        .edit-item, .remove-item {
            cursor: pointer;
            margin-left: 5px;
        }
        .edit-item {
            color: #2196F3;
        }
        .remove-item {
            color: #F44336;
        }
        .properties-panel {
            display: none;
            position: fixed;
            top: 20px;
            right: 20px;
            background: white;
            border-radius: 5px;
            padding: 15px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.2);
            width: 350px;
            z-index: 1000;
            max-height: 80vh;
            overflow-y: auto;
        }
        .properties-header {
            font-weight: bold;
            margin-bottom: 15px;
            border-bottom: 1px solid #eee;
            padding-bottom: 10px;
            position: relative;
        }
        .close-panel {
            position: absolute;
            right: 0;
            top: 0;
            cursor: pointer;
        }
        .property-field {
            margin-bottom: 10px;
        }
        .property-field label {
            display: block;
            margin-bottom: 5px;
            font-weight: bold;
        }
        .property-field input, .property-field select, .property-field textarea {
            width: 100%;
            padding: 8px;
            border: 1px solid #ddd;
            border-radius: 3px;
        }
        .property-field input[type="checkbox"] {
            width: auto;
        }
        .json-output {
            margin-top: 20px;
            background: #272822;
            color: #f8f8f2;
            padding: 15px;
            border-radius: 5px;
            display: none;
            max-height: 400px;
            overflow: auto;
            white-space: pre-wrap;
            font-family: monospace;
        }
        .tabs {
            display: flex;
            border-bottom: 1px solid #ddd;
            margin-bottom: 15px;
        }
        .tab {
            padding: 8px 15px;
            cursor: pointer;
            border: 1px solid transparent;
            border-bottom: none;
            margin-right: 5px;
        }
        .tab.active {
            background: #fff;
            border-color: #ddd;
            border-radius: 5px 5px 0 0;
        }
        .tab-content {
            display: none;
        }
        .tab-content.active {
            display: block;
        }
        .id-display {
            font-size: 0.8em;
            color: #888;
            margin-bottom: 10px;
            border: 1px solid #eee;
            padding: 5px;
            background: #f9f9f9;
        }
        .custom-property-row {
            display: flex;
            margin-bottom: 5px;
        }
        .custom-property-row input {
            flex: 1;
            margin-right: 5px;
        }
        .remove-prop {
            cursor: pointer;
            color: #F44336;
            font-weight: bold;
        }
        .add-property-btn {
            margin-top: 5px;
            background: #2196F3;
        }
        .catalog-item-details {
            position: relative;
        }
        .reference-json {
            margin-top: 20px;
            padding: 15px;
            background: #fff;
            border-radius: 5px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
    </style>
</head>
<body>
    <h1>Metadata Form Builder with References</h1>
    <p>Drag and drop sections and fields to build your form. Each section and field keeps its original reference ID.</p>
    
    <div class="container">
        <!-- Catalog Panel -->
        <div class="catalog-panel">
            <div class="catalog">
                <div class="catalog-header">Sections <span class="add-new" id="add-section">+</span></div>
                <div id="section-catalog">
                    <div class="section-catalog-item" data-type="section" data-name="Basic Info" data-id="SEC001">
                        <div class="catalog-item-details">
                            Basic Info
                            <span class="catalog-item-id">ID: SEC001</span>
                        </div>
                    </div>
                    <div class="section-catalog-item" data-type="section" data-name="Contact Details" data-id="SEC002">
                        <div class="catalog-item-details">
                            Contact Details
                            <span class="catalog-item-id">ID: SEC002</span>
                        </div>
                    </div>
                    <div class="section-catalog-item" data-type="section" data-name="Additional Info" data-id="SEC003">
                        <div class="catalog-item-details">
                            Additional Info
                            <span class="catalog-item-id">ID: SEC003</span>
                        </div>
                    </div>
                </div>
            </div>
            
            <div class="catalog">
                <div class="catalog-header">Fields <span class="add-new" id="add-field">+</span></div>
                <div id="field-catalog">
                    <div class="field-catalog-item" data-type="text" data-name="Full Name" data-id="FLD001">
                        <div class="catalog-item-details">
                            Full Name <span class="field-type">(Text)</span>
                            <span class="catalog-item-id">ID: FLD001</span>
                        </div>
                    </div>
                    <div class="field-catalog-item" data-type="email" data-name="Email" data-id="FLD002">
                        <div class="catalog-item-details">
                            Email <span class="field-type">(Email)</span>
                            <span class="catalog-item-id">ID: FLD002</span>
                        </div>
                    </div>
                    <div class="field-catalog-item" data-type="date" data-name="Date of Birth" data-id="FLD003">
                        <div class="catalog-item-details">
                            Date of Birth <span class="field-type">(Date)</span>
                            <span class="catalog-item-id">ID: FLD003</span>
                        </div>
                    </div>
                    <div class="field-catalog-item" data-type="select" data-name="Status" data-id="FLD004">
                        <div class="catalog-item-details">
                            Status <span class="field-type">(Select)</span>
                            <span class="catalog-item-id">ID: FLD004</span>
                        </div>
                    </div>
                    <div class="field-catalog-item" data-type="number" data-name="ID" data-id="FLD005">
                        <div class="catalog-item-details">
                            ID <span class="field-type">(Number)</span>
                            <span class="catalog-item-id">ID: FLD005</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Form Container -->
        <div class="form-container" id="form-container">
            <div class="empty-message">Drag sections from the catalog to begin building your form.</div>
        </div>
    </div>
    
    <div class="buttons">
        <button id="generate-json">Generate JSON</button>
        <button id="clear-form">Clear Form</button>
    </div>
    
    <div class="json-output" id="json-output"></div>
    
    <!-- Properties Panel -->
    <div class="properties-panel" id="properties-panel">
        <div class="properties-header">
            Edit Properties <span class="close-panel" id="close-panel">×</span>
        </div>
        
        <div class="id-display" id="reference-id-display">Reference ID: <span id="ref-id-value">SEC001</span></div>
        <div class="id-display" id="instance-id-display">Instance ID: <span id="instance-id-value">instance_123</span></div>
        
        <div class="tabs">
            <div class="tab active" data-tab="basic">Basic</div>
            <div class="tab" data-tab="custom">Override Properties</div>
        </div>
        
        <div class="tab-content active" data-tab="basic">
            <div class="property-field">
                <label for="prop-name">Name:</label>
                <input type="text" id="prop-name">
            </div>
            <div class="property-field">
                <label for="prop-label">Label:</label>
                <input type="text" id="prop-label">
            </div>
            <div class="property-field" id="prop-type-field">
                <label for="prop-type">Type:</label>
                <select id="prop-type">
                    <option value="text">Text</option>
                    <option value="email">Email</option>
                    <option value="number">Number</option>
                    <option value="date">Date</option>
                    <option value="select">Select</option>
                    <option value="checkbox">Checkbox</option>
                </select>
            </div>
            <div class="property-field">
                <label for="prop-required">Required:</label>
                <input type="checkbox" id="prop-required">
            </div>
            <div class="property-field">
                <label for="prop-visible">Visible:</label>
                <input type="checkbox" id="prop-visible" checked>
            </div>
            <div class="property-field">
                <label for="prop-editable">Editable:</label>
                <input type="checkbox" id="prop-editable" checked>
            </div>
        </div>
        
        <div class="tab-content" data-tab="custom">
            <div class="property-field">
                <label>Custom Override Properties:</label>
                <div id="custom-properties-container">
                    <!-- Custom properties will be added here -->
                </div>
                <button class="add-property-btn" id="add-property-btn">Add Property</button>
            </div>
        </div>
        
        <button id="save-properties">Save</button>
    </div>
    
    <script>
        $(document).ready(function() {
            // Initialize variables
            let formData = [];
            let instanceCounter = 1;
            let currentEditItem = null;
            let fieldCounter = 6;  // Starting after FLD005
            let sectionCounter = 4;  // Starting after SEC003
            
            // Make catalog items draggable
            $(".section-catalog-item, .field-catalog-item").draggable({
                helper: "clone",
                cursor: "move",
                cursorAt: { top: 15, left: 50 },
                zIndex: 100
            });
            
            // Make form container droppable for sections
            $("#form-container").droppable({
                accept: ".section-catalog-item",
                hoverClass: "dropzone-active",
                drop: function(event, ui) {
                    if ($(this).find(".empty-message").length) {
                        $(this).find(".empty-message").remove();
                    }
                    
                    let sectionName = ui.draggable.data('name');
                    let referenceId = ui.draggable.data('id');
                    let instanceId = "instance_" + instanceCounter++;
                    
                    let sectionData = {
                        instanceId: instanceId,
                        referenceId: referenceId,
                        type: "section",
                        name: sectionName,
                        label: sectionName,
                        fields: [],
                        subSections: [],
                        overrideProperties: {}
                    };
                    
                    formData.push(sectionData);
                    
                    let sectionHtml = `
                        <div class="section" id="${instanceId}" data-id="${instanceId}" data-reference-id="${referenceId}">
                            <div class="section-header">
                                <div class="section-title">
                                    ${sectionName}
                                    <span class="section-id">Ref ID: ${referenceId} | Instance ID: ${instanceId}</span>
                                </div>
                                <div>
                                    <button class="add-section-btn" data-id="${instanceId}">Add Subsection</button>
                                    <span class="edit-item" data-id="${instanceId}" data-type="section">✏️</span>
                                    <span class="remove-item" data-id="${instanceId}">🗑️</span>
                                </div>
                            </div>
                            <div class="section-drop-area" data-section-id="${instanceId}">
                                <div class="empty-message">Drop fields or sections here</div>
                            </div>
                        </div>
                    `;
                    
                    $(this).append(sectionHtml);
                    initializeDroppable();
                    bindEvents();
                }
            });
            
            // Function to initialize droppable areas
            function initializeDroppable() {
                // Make section drop areas droppable for fields and sections
                $(".section-drop-area, .nested-section-drop-area").droppable({
                    accept: ".section-catalog-item, .field-catalog-item",
                    hoverClass: "dropzone-active",
                    drop: function(event, ui) {
                        let sectionId = $(this).data('section-id');
                        let itemType = ui.draggable.data('type');
                        let itemName = ui.draggable.data('name');
                        let referenceId = ui.draggable.data('id');
                        
                        if ($(this).find(".empty-message").length) {
                            $(this).find(".empty-message").remove();
                        }
                        
                        // Find the parent section in formData
                        let parentSection = findSectionById(sectionId);
                        
                        if (itemType === 'section') {
                            // Adding a subsection
                            let instanceId = "instance_" + instanceCounter++;
                            
                            let subSectionData = {
                                instanceId: instanceId,
                                referenceId: referenceId,
                                type: "section",
                                name: itemName,
                                label: itemName,
                                fields: [],
                                subSections: [],
                                overrideProperties: {}
                            };
                            
                            if (parentSection) {
                                parentSection.subSections.push(subSectionData);
                            }
                            
                            let subSectionHtml = `
                                <div class="nested-section" id="${instanceId}" data-id="${instanceId}" data-reference-id="${referenceId}">
                                    <div class="section-header">
                                        <div class="section-title">
                                            ${itemName}
                                            <span class="section-id">Ref ID: ${referenceId} | Instance ID: ${instanceId}</span>
                                        </div>
                                        <div>
                                            <button class="add-section-btn" data-id="${instanceId}">Add Subsection</button>
                                            <span class="edit-item" data-id="${instanceId}" data-type="section">✏️</span>
                                            <span class="remove-item" data-id="${instanceId}">🗑️</span>
                                        </div>
                                    </div>
                                    <div class="nested-section-drop-area" data-section-id="${instanceId}">
                                        <div class="empty-message">Drop fields or sections here</div>
                                    </div>
                                </div>
                            `;
                            
                            $(this).append(subSectionHtml);
                        } else {
                            // Adding a field
                            let instanceId = "instance_" + instanceCounter++;
                            let fieldType = ui.draggable.data('type');
                            
                            let fieldData = {
                                instanceId: instanceId,
                                referenceId: referenceId,
                                type: fieldType,
                                name: itemName,
                                label: itemName,
                                overrideProperties: {}
                            };
                            
                            if (fieldType === 'select') {
                                fieldData.options = ['Option 1', 'Option 2', 'Option 3'];
                            }
                            
                            if (parentSection) {
                                parentSection.fields.push(fieldData);
                            }
                            
                            let fieldHtml = `
                                <div class="field-item" data-id="${instanceId}" data-reference-id="${referenceId}">
                                    ${itemName} <span class="field-type">(${fieldType})</span>
                                    <span class="field-id">Ref ID: ${referenceId} | Instance ID: ${instanceId}</span>
                                    <div class="item-actions">
                                        <span class="edit-item" data-id="${instanceId}" data-type="field">✏️</span>
                                        <span class="remove-item" data-id="${instanceId}">🗑️</span>
                                    </div>
                                </div>
                            `;
                            
                            $(this).append(fieldHtml);
                        }
                        
                        initializeDroppable();
                        bindEvents();
                    }
                });
            }
            
            // Function to find a section by ID in formData
            function findSectionById(id, data = formData) {
                for (let i = 0; i < data.length; i++) {
                    if (data[i].instanceId === id) {
                        return data[i];
                    }
                    
                    // Check in subsections
                    if (data[i].subSections && data[i].subSections.length > 0) {
                        let found = findSectionById(id, data[i].subSections);
                        if (found) return found;
                    }
                }
                return null;
            }
            
            // Function to find a field by ID in formData
            function findFieldById(id, data = formData) {
                for (let i = 0; i < data.length; i++) {
                    // Check in fields
                    for (let j = 0; j < data[i].fields.length; j++) {
                        if (data[i].fields[j].instanceId === id) {
                            return {
                                field: data[i].fields[j],
                                parentArray: data[i].fields,
                                index: j,
                                parentSection: data[i]
                            };
                        }
                    }
                    
                    // Check in subsections
                    if (data[i].subSections && data[i].subSections.length > 0) {
                        let found = findFieldById(id, data[i].subSections);
                        if (found) return found;
                    }
                }
                return null;
            }
            
            // Function to find an item (section or field) by instance ID
            function findItemById(id) {
                // Try to find as section first
                let section = findSectionById(id);
                if (section) {
                    return { item: section, type: 'section' };
                }
                
                // Then try to find as field
                let fieldInfo = findFieldById(id);
                if (fieldInfo) {
                    return { item: fieldInfo.field, type: 'field' };
                }
                
                return null;
            }
            
            // Function to remove an item by ID from formData
            function removeItemById(id, data = formData) {
                for (let i = 0; i < data.length; i++) {
                    if (data[i].instanceId === id) {
                        data.splice(i, 1);
                        return true;
                    }
                    
                    // Check in fields
                    for (let j = 0; j < data[i].fields.length; j++) {
                        if (data[i].fields[j].instanceId === id) {
                            data[i].fields.splice(j, 1);
                            return true;
                        }
                    }
                    
                    // Check in subsections
                    if (data[i].subSections && data[i].subSections.length > 0) {
                        let removed = removeItemById(id, data[i].subSections);
                        if (removed) return true;
                    }
                }
                return false;
            }
            
            // Bind event handlers
            function bindEvents() {
                // Add section button
                $(".add-section-btn").off('click').on('click', function() {
                    let parentId = $(this).data('id');
                    
                    // Open dialog to create new section
                    let sectionName = prompt("Enter subsection name:", "New Subsection");
                    if (!sectionName) return;
                    
                    let referenceId = "SEC" + String(sectionCounter++).padStart(3, '0');
                    let instanceId = "instance_" + instanceCounter++;
                    
                    let subSectionData = {
                        instanceId: instanceId,
                        referenceId: referenceId,
                        type: "section",
                        name: sectionName,
                        label: sectionName,
                        fields: [],
                        subSections: [],
                        overrideProperties: {}
                    };
                    
                    let parentSection = findSectionById(parentId);
                    if (parentSection) {
                        parentSection.subSections.push(subSectionData);
                    }
                    
                    let subSectionHtml = `
                        <div class="nested-section" id="${instanceId}" data-id="${instanceId}" data-reference-id="${referenceId}">
                            <div class="section-header">
                                <div class="section-title">
                                    ${sectionName}
                                    <span class="section-id">Ref ID: ${referenceId} | Instance ID: ${instanceId}</span>
                                </div>
                                <div>
                                    <button class="add-section-btn" data-id="${instanceId}">Add Subsection</button>
                                    <span class="edit-item" data-id="${instanceId}" data-type="section">✏️</span>
                                    <span class="remove-item" data-id="${instanceId}">🗑️</span>
                                </div>
                            </div>
                            <div class="nested-section-drop-area" data-section-id="${instanceId}">
                                <div class="empty-message">Drop fields or sections here</div>
                            </div>
                        </div>
                    `;
                    
                    // Find the drop area to append the new section
                    let dropArea = $(`#${parentId}`).find('.section-drop-area, .nested-section-drop-area').first();
                    
                    if (dropArea.find(".empty-message").length) {
                        dropArea.find(".empty-message").remove();
                    }
                    
                    dropArea.append(subSectionHtml);
                    initializeDroppable();
                    bindEvents();
                });
                
                // Edit item button
                $(".edit-item").off('click').on('click', function() {
                    let itemId = $(this).data('id');
                    let itemType = $(this).data('type');
                    
                    let itemInfo = findItemById(itemId);
                    if (itemInfo) {
                        openPropertiesPanel(itemInfo.item, itemType);
                    }
                });
                
                // Remove item button
                $(".remove-item").off('click').on('click', function() {
                    let itemId = $(this).data('id');
                    let element = $(`#${itemId}`);
                    
                    // If it's not a direct ID element, find the parent with data-id
                    if (element.length === 0) {
                        element = $(this).closest('[data-id]');
                    }
                    
                    if (element.length) {
                        // Remove from DOM
                        element.remove();
                        
                        // Remove from data structure
                        removeItemById(itemId);
                        
                        // Check if form is now empty
                        if ($("#form-container").children().length === 0) {
                            $("#form-container").html('<div class="empty-message">Drag sections from the catalog to begin building your form.</div>');
                        }
                    }
                });
            }
            
            // Function to populate custom properties in the properties panel
            function populateCustomProperties(item) {
                let container = $("#custom-properties-container");
                container.empty();
                
                if (item.overrideProperties) {
                    Object.entries(item.overrideProperties).forEach(([key, value]) => {
                        addCustomPropertyRow(key, value);
                    });
                }
            }
            
            // Function to add a custom property row to the properties panel
            function addCustomPropertyRow(key = '', value = '') {
                let container = $("#custom-properties-container");
                
                let row = $(`
                    <div class="custom-property-row">
                        <input type="text" class="prop-key" placeholder="Property name" value="${key}">
                        <input type="text" class="prop-value" placeholder="Value" value="${value}">
                        <span class="remove-prop">×</span>
                    </div>
                `);
                
                // Bind remove event
                row.find('.remove-prop').on('click', function() {
                    $(this).closest('.custom-property-row').remove();
                });
                
                container.append(row);
            }
            
            // Function to open properties panel
            function openPropertiesPanel(item, itemType) {
                currentEditItem = { item, itemType };
                
                // Set the reference and instance IDs
                $("#ref-id-value").text(item.referenceId);
                $("#instance-id-value").text(item.instanceId);
                
                // Set values in the panel
                $("#prop-name").val(item.name || '');
                $("#prop-label").val(item.label || '');
                $("#prop-required").prop('checked', item.required || false);
                $("#prop-visible").prop('checked', item.visible !== false);
                $("#prop-editable").prop('checked', item.editable !== false);
                
                // Show/hide type field based on item type
                if (itemType === 'field') {
                    $("#prop-type-field").show();
                    $("#prop-type").val(item.type || 'text');
                } else {
                    $("#prop-type-field").hide();
                }
                
                // Populate override properties
                populateCustomProperties(item);
                
                // Show the panel
                $("#properties-panel").show();
            }
            
            // Close properties panel
            $("#close-panel").click(function() {
                $("#properties-panel").hide();
                currentEditItem = null;
            });
            
            // Tab switching in properties panel
            $(".tab").click(function() {
                $(".tab").removeClass('active');
                $(this).addClass('active');
                
                let tabId = $(this).data('tab');
                $(".tab-content").removeClass('active');
                $(`.tab-content[data-tab="${tabId}"]`).addClass('active');
            });
            
            // Add property button
            $("#add-property-btn").click(function() {
                addCustomPropertyRow();
            });
            
            // Save properties
            $("#save-properties").click(function() {
                if (!currentEditItem) return;
                
                let { item, itemType } = currentEditItem;
                
                // Update basic properties
                item.name = $("#prop-name").val();
                item.label = $("#prop-label").val();
                
                // Update custom/override properties
                item.overrideProperties = {};
                
                $(".custom-property-row").each(function() {
                    let key = $(this).find('.prop-key').val().trim();
                    let value = $(this).find('.prop-value').val().trim();
                    
                    if (key) {
                        item.overrideProperties[key] = value;
                    }
                });
                
                // Update specific properties
                if ($("#prop-required").is(':checked')) {
                    item.overrideProperties.required = true;
                }
                
                if (!$("#prop-visible").is(':checked')) {
                    item.overrideProperties.visible = false;
                }
                
                if (!$("#prop-editable").is(':checked')) {
                    item.overrideProperties.editable = false;
                }
                
                if (itemType === 'field') {
                    let newType = $("#prop-type").val();
                    if (newType !== item.type) {
                        item.overrideProperties.type = newType;
                    }
                }
                
                // Update UI
                updateItemDisplay(item, itemType);
                
                // Close the panel
                $("#properties-panel").hide();
                currentEditItem = null;
            });
            
            // Function to update item display after editing
            function updateItemDisplay(item, itemType) {
                let element;
                
                if (itemType === 'section') {
                    element = $(`#${item.instanceId}`).find('.section-title').first();
                    element.contents().filter(function() {
                        return this.nodeType === 3; // Text nodes only
                    }).first().replaceWith(item.label);
                } else if (itemType === 'field') {
                    element = $(`.field-item[data-id="${item.instanceId}"]`);
                    
                    // Update label and type display
                    let displayType = item.overrideProperties.type || item.type;
                    
                    element.contents().filter(function() {
                        return this.nodeType === 3; // Text nodes only
                    }).first().replaceWith(`${item.label} `);
                    
                    element.find('.field-type').text(`(${displayType})`);
                }
            }
            
            // Function to rebuild UI from JSON data
            function rebuildFormFromJson(jsonData) {
                // Clear existing form
                formData = [];
                $("#form-container").empty();
                
                if (!jsonData || jsonData.length === 0) {
                    $("#form-container").html('<div class="empty-message">Drag sections from the catalog to begin building your form.</div>');
                    return;
                }
                
                // Helper function to recursively build the UI
                function buildSectionUI(sectionData, parentElement) {
                    let sectionId = sectionData.instanceId;
                    let referenceId = sectionData.sectionId;
                    
                    // Find section definition in catalog
                    let catalogSection = findCatalogItem(referenceId, 'section');
                    if (!catalogSection) {
                        console.error(`Section reference not found: ${referenceId}`);
                        return;
                    }
                    
                    // Create section data object
                    let sectionObj = {
                        instanceId: sectionId,
                        referenceId: referenceId,
                        type: "section",
                        name: catalogSection.name,
                        label: catalogSection.name,
                        fields: [],
                        subSections: [],
                        overrideProperties: sectionData.overrideProperties || {}
                    };
                    
                    // Apply override properties
                    if (sectionData.overrideProperties) {
                        if (sectionData.overrideProperties.name) sectionObj.name = sectionData.overrideProperties.name;
                        if (sectionData.overrideProperties.label) sectionObj.label = sectionData.overrideProperties.label;
                    }
                    
                    // Add to form data
                    if (parentElement.is("#form-container")) {
                        formData.push(sectionObj);
                    } else {
                        // Find parent section in data structure
                        let parentId = parentElement.closest('.section, .nested-section').attr('id');
                        let parentSection = findSectionById(parentId);
                        if (parentSection) {
                            parentSection.subSections.push(sectionObj);
                        }
                    }
                    
                    // Create HTML
                    let isNested = !parentElement.is("#form-container");
                    let sectionHtml = isNested ? 
                        `<div class="nested-section" id="${sectionId}" data-id="${sectionId}" data-reference-id="${referenceId}">
                            <div class="section-header">
                                <div class="section-title">
                                    ${sectionObj.label}
                                    <span class="section-id">Ref ID: ${referenceId} | Instance ID: ${sectionId}</span>
                                </div>
                                <div>
                                    <button class="add-section-btn" data-id="${sectionId}">Add Subsection</button>
                                    <span class="edit-item" data-id="${sectionId}" data-type="section">✏️</span>
                                    <span class="remove-item" data-id="${sectionId}">🗑️</span>
                                </div>
                            </div>
                            <div class="nested-section-drop-area" data-section-id="${sectionId}">
                                <div class="empty-message">Drop fields or sections here</div>
                            </div>
                        </div>` :
                        `<div class="section" id="${sectionId}" data-id="${sectionId}" data-reference-id="${referenceId}">
                            <div class="section-header">
                                <div class="section-title">
                                    ${sectionObj.label}
                                    <span class="section-id">Ref ID: ${referenceId} | Instance ID: ${sectionId}</span>
                                </div>
                                <div>
                                    <button class="add-section-btn" data-id="${sectionId}">Add Subsection</button>
                                    <span class="edit-item" data-id="${sectionId}" data-type="section">✏️</span>
                                    <span class="remove-item" data-id="${sectionId}">🗑️</span>
                                </div>
                            </div>
                            <div class="section-drop-area" data-section-id="${sectionId}">
                                <div class="empty-message">Drop fields or sections here</div>
                            </div>
                        </div>`;
                    
                    parentElement.append(sectionHtml);
                    
                    // Add fields if they exist
                    let dropArea = $(`#${sectionId}`).find('.section-drop-area, .nested-section-drop-area').first();
                    
                    if (sectionData.fields && sectionData.fields.length > 0) {
                        dropArea.find(".empty-message").remove();
                        
                        sectionData.fields.forEach(fieldData => {
                            // Find field definition in catalog
                            let catalogField = findCatalogItem(fieldData.fieldId, 'field');
                            if (!catalogField) {
                                console.error(`Field reference not found: ${fieldData.fieldId}`);
                                return;
                            }
                            
                            let fieldId = fieldData.instanceId;
                            let fieldType = catalogField.type;
                            
                            // Create field data object
                            let fieldObj = {
                                instanceId: fieldId,
                                referenceId: fieldData.fieldId,
                                type: fieldType,
                                name: catalogField.name,
                                label: catalogField.name,
                                overrideProperties: fieldData.overrideProperties || {}
                            };
                            
                            // Apply override properties
                            if (fieldData.overrideProperties) {
                                if (fieldData.overrideProperties.name) fieldObj.name = fieldData.overrideProperties.name;
                                if (fieldData.overrideProperties.label) fieldObj.label = fieldData.overrideProperties.label;
                                if (fieldData.overrideProperties.type) fieldObj.type = fieldData.overrideProperties.type;
                            }
                            
                            // Add to section data
                            sectionObj.fields.push(fieldObj);
                            
                            // Create field HTML
                            let fieldHtml = `
                                <div class="field-item" data-id="${fieldId}" data-reference-id="${fieldData.fieldId}">
                                    ${fieldObj.label} <span class="field-type">(${fieldObj.type})</span>
                                    <span class="field-id">Ref ID: ${fieldData.fieldId} | Instance ID: ${fieldId}</span>
                                    <div class="item-actions">
                                        <span class="edit-item" data-id="${fieldId}" data-type="field">✏️</span>
                                        <span class="remove-item" data-id="${fieldId}">🗑️</span>
                                    </div>
                                </div>
                            `;
                            
                            dropArea.append(fieldHtml);
                        });
                    }
                    
                    // Process subsections recursively
                    if (sectionData.subSections && sectionData.subSections.length > 0) {
                        dropArea.find(".empty-message").remove();
                        
                        sectionData.subSections.forEach(subSection => {
                            buildSectionUI(subSection, dropArea);
                        });
                    }
                }
                
                // Process each top-level section
                jsonData.forEach(section => {
                    buildSectionUI(section, $("#form-container"));
                });
                
                // Reinitialize the interactive elements
                initializeDroppable();
                bindEvents();
            }
            
            // Helper function to find an item in the catalog by ID
            function findCatalogItem(id, type) {
                let selector = type === 'section' ? '.section-catalog-item' : '.field-catalog-item';
                let result = null;
                
                $(selector).each(function() {
                    if ($(this).data('id') === id) {
                        result = {
                            id: $(this).data('id'),
                            name: $(this).data('name'),
                            type: $(this).data('type')
                        };
                        return false; // Break the loop
                    }
                });
                
                return result;
            }
            
            // Generate JSON
            $("#generate-json").click(function() {
                function prepareJson(items) {
                    return items.map(item => {
                        let jsonItem = {
                            sectionId: item.referenceId,
                            instanceId: item.instanceId
                        };
                        
                        // Add override properties if they exist
                        if (Object.keys(item.overrideProperties).length > 0) {
                            jsonItem.overrideProperties = { ...item.overrideProperties };
                        }
                        
                        // Add fields if they exist
                        if (item.fields && item.fields.length > 0) {
                            jsonItem.fields = item.fields.map(field => ({
                                fieldId: field.referenceId,
                                instanceId: field.instanceId,
                                overrideProperties: Object.keys(field.overrideProperties).length > 0 ? 
                                    { ...field.overrideProperties } : undefined
                            })).filter(f => f.overrideProperties || true);
                        }
                        
                        // Add subsections if they exist
                        if (item.subSections && item.subSections.length > 0) {
                            jsonItem.subSections = prepareJson(item.subSections);
                        }
                        
                        return jsonItem;
                    });
                }
                
                let jsonData = prepareJson(formData);
                let jsonOutput = JSON.stringify(jsonData, null, 2);
                
                $("#json-output").text(jsonOutput).show();
                
                // Simulate saving to DB
                localStorage.setItem('formBuilderData', jsonOutput);
                alert("Form data saved! You can now reload the page and use 'Load Saved Form' to rebuild the form.");
                
                console.log("Form structure with references:", jsonData);
            });
            
            // Clear form
            $("#clear-form").click(function() {
                formData = [];
                $("#form-container").html('<div class="empty-message">Drag sections from the catalog to begin building your form.</div>');
                $("#json-output").hide();
            });
            
            // Add a button to load saved form
            $(".buttons").append(`<button id="load-saved-form">Load Saved Form</button>`);
            
            // Load saved form button
            $("#load-saved-form").click(function() {
                let savedData = localStorage.getItem('formBuilderData');
                if (savedData) {
                    try {
                        let jsonData = JSON.parse(savedData);
                        rebuildFormFromJson(jsonData);
                    } catch (e) {
                        console.error("Error parsing saved form data:", e);
                        alert("Error loading saved form data");
                    }
                } else {
                    alert("No saved form data found");
                }
            });
            
            // Add new section to catalog
            $("#add-section").click(function() {
                // Instead of prompt, show a proper form dialog
                $("#section-dialog").show();
            });
            
            // Add new field to catalog
            $("#add-field").click(function() {
                // Instead of prompt, show a proper form dialog
                $("#field-dialog").show();
            });
            
            // Initialize droppable areas
            initializeDroppable();
            
            // Bind events
            bindEvents();
        });
    </script>
    
    <!-- Section Creation Dialog -->
    <div id="section-dialog" class="properties-panel" style="position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 450px; z-index: 2000; display: none;">
        <div class="properties-header">
            Create New Section <span class="close-panel" id="close-section-dialog">×</span>
        </div>
        
        <div class="tabs">
            <div class="tab active" data-tab="section-basic">Basic</div>
            <div class="tab" data-tab="section-custom">Custom Properties</div>
        </div>
        
        <div class="tab-content active" data-tab="section-basic">
            <div class="property-field">
                <label for="new-section-id">Section ID:</label>
                <input type="text" id="new-section-id" readonly>
            </div>
            <div class="property-field">
                <label for="new-section-name">Section Name:</label>
                <input type="text" id="new-section-name" placeholder="Enter section name">
            </div>
            <div class="property-field">
                <label for="new-section-label">Display Label:</label>
                <input type="text" id="new-section-label" placeholder="Enter display label">
            </div>
            <div class="property-field">
                <label for="new-section-description">Description:</label>
                <textarea id="new-section-description" placeholder="Enter description"></textarea>
            </div>
            <div class="property-field">
                <label for="new-section-required">Required:</label>
                <input type="checkbox" id="new-section-required">
            </div>
            <div class="property-field">
                <label for="new-section-visible">Visible:</label>
                <input type="checkbox" id="new-section-visible" checked>
            </div>
            <div class="property-field">
                <label for="new-section-editable">Editable:</label>
                <input type="checkbox" id="new-section-editable" checked>
            </div>
        </div>
        
        <div class="tab-content" data-tab="section-custom">
            <div class="property-field">
                <label>Custom Properties:</label>
                <div id="section-custom-properties-container">
                    <!-- Custom properties will be added here -->
                </div>
                <button class="add-property-btn" id="add-section-property-btn">Add Property</button>
            </div>
        </div>
        
        <button id="create-section-btn">Create Section</button>
    </div>
    
    <!-- Field Creation Dialog -->
    <div id="field-dialog" class="properties-panel" style="position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 450px; z-index: 2000; display: none;">
        <div class="properties-header">
            Create New Field <span class="close-panel" id="close-field-dialog">×</span>
        </div>
        
        <div class="tabs">
            <div class="tab active" data-tab="field-basic">Basic</div>
            <div class="tab" data-tab="field-validation">Validation</div>
            <div class="tab" data-tab="field-custom">Custom Properties</div>
        </div>
        
        <div class="tab-content active" data-tab="field-basic">
            <div class="property-field">
                <label for="new-field-id">Field ID:</label>
                <input type="text" id="new-field-id" readonly>
            </div>
            <div class="property-field">
                <label for="new-field-name">Field Name:</label>
                <input type="text" id="new-field-name" placeholder="Enter field name">
            </div>
            <div class="property-field">
                <label for="new-field-label">Display Label:</label>
                <input type="text" id="new-field-label" placeholder="Enter display label">
            </div>
            <div class="property-field">
                <label for="new-field-type">Field Type:</label>
                <select id="new-field-type">
                    <option value="text">Text</option>
                    <option value="email">Email</option>
                    <option value="number">Number</option>
                    <option value="date">Date</option>
                    <option value="datetime">Date & Time</option>
                    <option value="select">Select</option>
                    <option value="multiselect">Multi-Select</option>
                    <option value="checkbox">Checkbox</option>
                    <option value="radio">Radio</option>
                    <option value="textarea">Text Area</option>
                    <option value="file">File Upload</option>
                    <option value="phone">Phone Number</option>
                    <option value="url">URL</option>
                </select>
            </div>
            <div class="property-field">
                <label for="new-field-description">Description:</label>
                <textarea id="new-field-description" placeholder="Enter description"></textarea>
            </div>
            <div class="property-field">
                <label for="new-field-placeholder">Placeholder:</label>
                <input type="text" id="new-field-placeholder" placeholder="Enter placeholder text">
            </div>
            <div class="property-field" id="new-field-options-container" style="display: none;">
                <label for="new-field-options">Options (comma separated):</label>
                <input type="text" id="new-field-options" placeholder="Option 1, Option 2, Option 3">
            </div>
            <div class="property-field" id="new-field-default-container">
                <label for="new-field-default">Default Value:</label>
                <input type="text" id="new-field-default" placeholder="Enter default value">
            </div>
            <div class="property-field">
                <label for="new-field-required">Required:</label>
                <input type="checkbox" id="new-field-required">
            </div>
            <div class="property-field">
                <label for="new-field-visible">Visible:</label>
                <input type="checkbox" id="new-field-visible" checked>
            </div>
            <div class="property-field">
                <label for="new-field-editable">Editable:</label>
                <input type="checkbox" id="new-field-editable" checked>
            </div>
        </div>
        
        <div class="tab-content" data-tab="field-validation">
            <div class="property-field">
                <label for="new-field-min-length">Minimum Length:</label>
                <input type="number" id="new-field-min-length" min="0">
            </div>
            <div class="property-field">
                <label for="new-field-max-length">Maximum Length:</label>
                <input type="number" id="new-field-max-length" min="0">
            </div>
            <div class="property-field" id="new-field-min-value-container">
                <label for="new-field-min-value">Minimum Value:</label>
                <input type="number" id="new-field-min-value">
            </div>
            <div class="property-field" id="new-field-max-value-container">
                <label for="new-field-max-value">Maximum Value:</label>
                <input type="number" id="new-field-max-value">
            </div>
            <div class="property-field">
                <label for="new-field-pattern">Validation Pattern (RegEx):</label>
                <input type="text" id="new-field-pattern" placeholder="e.g. ^[A-Za-z0-9]+$">
            </div>
            <div class="property-field">
                <label for="new-field-error-message">Error Message:</label>
                <input type="text" id="new-field-error-message" placeholder="Enter custom error message">
            </div>
        </div>
        
        <div class="tab-content" data-tab="field-custom">
            <div class="property-field">
                <label>Custom Properties:</label>
                <div id="field-custom-properties-container">
                    <!-- Custom properties will be added here -->
                </div>
                <button class="add-property-btn" id="add-field-property-btn">Add Property</button>
            </div>
        </div>
        
        <button id="create-field-btn">Create Field</button>
    </div>
    
            <script>
        $(document).ready(function() {
            // Section dialog functionality
            $("#close-section-dialog").click(function() {
                $("#section-dialog").hide();
            });
            
            // Section tabs
            $("#section-dialog .tab").click(function() {
                $("#section-dialog .tab").removeClass('active');
                $(this).addClass('active');
                
                let tabId = $(this).data('tab');
                $("#section-dialog .tab-content").removeClass('active');
                $(`#section-dialog .tab-content[data-tab="${tabId}"]`).addClass('active');
            });
            
            // Field tabs
            $("#field-dialog .tab").click(function() {
                $("#field-dialog .tab").removeClass('active');
                $(this).addClass('active');
                
                let tabId = $(this).data('tab');
                $("#field-dialog .tab-content").removeClass('active');
                $(`#field-dialog .tab-content[data-tab="${tabId}"]`).addClass('active');
            });
            
            // Function to add custom property row to section creation form
            function addSectionCustomProperty(key = '', value = '') {
                let container = $("#section-custom-properties-container");
                let row = $(`
                    <div class="custom-property-row">
                        <input type="text" class="prop-key" placeholder="Property name" value="${key}">
                        <input type="text" class="prop-value" placeholder="Value" value="${value}">
                        <span class="remove-prop">×</span>
                    </div>
                `);
                
                row.find('.remove-prop').on('click', function() {
                    $(this).closest('.custom-property-row').remove();
                });
                
                container.append(row);
            }
            
            // Add section property button
            $("#add-section-property-btn").click(function() {
                addSectionCustomProperty();
            });
            
            // Function to add custom property row to field creation form
            function addFieldCustomProperty(key = '', value = '') {
                let container = $("#field-custom-properties-container");
                let row = $(`
                    <div class="custom-property-row">
                        <input type="text" class="prop-key" placeholder="Property name" value="${key}">
                        <input type="text" class="prop-value" placeholder="Value" value="${value}">
                        <span class="remove-prop">×</span>
                    </div>
                `);
                
                row.find('.remove-prop').on('click', function() {
                    $(this).closest('.custom-property-row').remove();
                });
                
                container.append(row);
            }
            
            // Add field property button
            $("#add-field-property-btn").click(function() {
                addFieldCustomProperty();
            });
            
            // Set the new section ID when opening the dialog
            $("#add-section").click(function() {
                let id = "SEC" + String(sectionCounter++).padStart(3, '0');
                $("#new-section-id").val(id);
                $("#new-section-name").val('');
                $("#new-section-label").val('');
                $("#new-section-description").val('');
                $("#new-section-required").prop('checked', false);
                $("#new-section-visible").prop('checked', true);
                $("#new-section-editable").prop('checked', true);
                $("#section-custom-properties-container").empty();
                
                // Reset to first tab
                $("#section-dialog .tab").removeClass('active');
                $("#section-dialog .tab[data-tab='section-basic']").addClass('active');
                $("#section-dialog .tab-content").removeClass('active');
                $("#section-dialog .tab-content[data-tab='section-basic']").addClass('active');
                
                $("#section-dialog").show();
            });
            
            // Create section button
            $("#create-section-btn").click(function() {
                let id = $("#new-section-id").val();
                let name = $("#new-section-name").val().trim();
                let label = $("#new-section-label").val().trim() || name;
                let description = $("#new-section-description").val().trim();
                let required = $("#new-section-required").is(':checked');
                let visible = $("#new-section-visible").is(':checked');
                let editable = $("#new-section-editable").is(':checked');
                
                if (!name) {
                    alert("Please enter a section name");
                    return;
                }
                
                // Collect custom properties
                let customProps = {};
                $("#section-custom-properties-container .custom-property-row").each(function() {
                    let key = $(this).find('.prop-key').val().trim();
                    let value = $(this).find('.prop-value').val().trim();
                    
                    if (key) {
                        customProps[key] = value;
                    }
                });
                
                // Store all properties as data attributes
                let dataAttributes = `data-type="section" data-name="${name}" data-id="${id}"
                                     data-label="${label}" data-description="${description}" 
                                     data-required="${required}" data-visible="${visible}" 
                                     data-editable="${editable}"`;
                
                // Add section to catalog
                let sectionHtml = `
                    <div class="section-catalog-item" ${dataAttributes}>
                        <div class="catalog-item-details">
                            ${label || name}
                            <span class="catalog-item-id">ID: ${id}</span>
                        </div>
                    </div>
                `;
                
                $("#section-catalog").append(sectionHtml);
                
                // Store custom properties in a data attribute for the new item
                let $newItem = $("#section-catalog .section-catalog-item").last();
                $newItem.data('customProperties', customProps);
                
                // Make the new item draggable
                $(".section-catalog-item").draggable({
                    helper: "clone",
                    cursor: "move",
                    cursorAt: { top: 15, left: 50 },
                    zIndex: 100
                });
                
                // Hide dialog
                $("#section-dialog").hide();
            });
            
            // Field dialog functionality
            $("#close-field-dialog").click(function() {
                $("#field-dialog").hide();
            });
            
            // Set the new field ID when opening the dialog
            $("#add-field").click(function() {
                let id = "FLD" + String(fieldCounter++).padStart(3, '0');
                $("#new-field-id").val(id);
                $("#new-field-name").val('');
                $("#new-field-label").val('');
                $("#new-field-type").val('text');
                $("#new-field-description").val('');
                $("#new-field-placeholder").val('');
                $("#new-field-options").val('');
                $("#new-field-default").val('');
                $("#new-field-required").prop('checked', false);
                $("#new-field-visible").prop('checked', true);
                $("#new-field-editable").prop('checked', true);
                $("#new-field-min-length").val('');
                $("#new-field-max-length").val('');
                $("#new-field-min-value").val('');
                $("#new-field-max-value").val('');
                $("#new-field-pattern").val('');
                $("#new-field-error-message").val('');
                $("#field-custom-properties-container").empty();
                
                // Hide options by default
                $("#new-field-options-container").hide();
                
                // Reset to first tab
                $("#field-dialog .tab").removeClass('active');
                $("#field-dialog .tab[data-tab='field-basic']").addClass('active');
                $("#field-dialog .tab-content").removeClass('active');
                $("#field-dialog .tab-content[data-tab='field-basic']").addClass('active');
                
                $("#field-dialog").show();
            });
            
            // Show/hide options field based on type
            $("#new-field-type").change(function() {
                let type = $(this).val();
                if (type === 'select' || type === 'multiselect' || type === 'radio') {
                    $("#new-field-options-container").show();
                } else {
                    $("#new-field-options-container").hide();
                }
                
                // Show/hide min/max value fields based on type
                if (type === 'number' || type === 'date' || type === 'datetime') {
                    $("#new-field-min-value-container, #new-field-max-value-container").show();
                } else {
                    $("#new-field-min-value-container, #new-field-max-value-container").hide();
                }
                
                // Adjust default value field
                if (type === 'checkbox') {
                    $("#new-field-default").attr('type', 'checkbox');
                } else {
                    $("#new-field-default").attr('type', 'text');
                }
            });
            
            // Create field button
            $("#create-field-btn").click(function() {
                let id = $("#new-field-id").val();
                let name = $("#new-field-name").val().trim();
                let label = $("#new-field-label").val().trim() || name;
                let type = $("#new-field-type").val();
                let description = $("#new-field-description").val().trim();
                let placeholder = $("#new-field-placeholder").val().trim();
                let options = $("#new-field-options").val().trim();
                let defaultValue = $("#new-field-default").val().trim();
                let required = $("#new-field-required").is(':checked');
                let visible = $("#new-field-visible").is(':checked');
                let editable = $("#new-field-editable").is(':checked');
                
                if (!name) {
                    alert("Please enter a field name");
                    return;
                }
                
                // Collect validation properties
                let validationProps = {
                    minLength: $("#new-field-min-length").val(),
                    maxLength: $("#new-field-max-length").val(),
                    minValue: $("#new-field-min-value").val(),
                    maxValue: $("#new-field-max-value").val(),
                    pattern: $("#new-field-pattern").val(),
                    errorMessage: $("#new-field-error-message").val()
                };
                
                // Collect custom properties
                let customProps = {};
                $("#field-custom-properties-container .custom-property-row").each(function() {
                    let key = $(this).find('.prop-key').val().trim();
                    let value = $(this).find('.prop-value').val().trim();
                    
                    if (key) {
                        customProps[key] = value;
                    }
                });
                
                // Store all properties as data attributes
                let dataAttributes = `data-type="${type}" data-name="${name}" data-id="${id}"
                                     data-label="${label}" data-description="${description}" 
                                     data-placeholder="${placeholder}" data-options="${options}"
                                     data-default="${defaultValue}" data-required="${required}" 
                                     data-visible="${visible}" data-editable="${editable}"`;
                
                // Add field to catalog
                let fieldHtml = `
                    <div class="field-catalog-item" ${dataAttributes}>
                        <div class="catalog-item-details">
                            ${label || name} <span class="field-type">(${type})</span>
                            <span class="catalog-item-id">ID: ${id}</span>
                        </div>
                    </div>
                `;
                
                $("#field-catalog").append(fieldHtml);
                
                // Store validation and custom properties in data attributes for the new item
                let $newItem = $("#field-catalog .field-catalog-item").last();
                $newItem.data('validationProperties', validationProps);
                $newItem.data('customProperties', customProps);
                
                // Make the new item draggable
                $(".field-catalog-item").draggable({
                    helper: "clone",
                    cursor: "move",
                    cursorAt: { top: 15, left: 50 },
                    zIndex: 100
                });
                
                // Hide dialog
                $("#field-dialog").hide();
            });
        });
    </script>
</body>
</html>
