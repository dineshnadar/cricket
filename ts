<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Test Case Management System</title>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css" integrity="sha512-iecdLmaskl7CVkqkXNQ/ZH/XLlvWZOJyj7Yy7tcenmpD1ypASozpmT/E0iPtmFIB46ZmdtAc9eNBvH0H/ZpiBw==" crossorigin="anonymous" referrerpolicy="no-referrer" />
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap/5.2.3/css/bootstrap.min.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/ag-grid/30.0.2/styles/ag-grid.min.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/ag-grid/30.0.2/styles/ag-theme-alpine.min.css">
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background-color: #f8f9fa;
        }
        .sidebar {
            position: fixed;
            top: 0;
            bottom: 0;
            left: 0;
            z-index: 100;
            padding: 48px 0 0;
            box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
            background-color: #187aba;
            color: white;
        }
        .sidebar-menu {
            padding-left: 0;
            list-style: none;
        }
        .sidebar-menu li {
            padding: 12px 20px;
            margin-bottom: 5px;
            cursor: pointer;
            border-left: 4px solid transparent;
            transition: all 0.2s ease;
        }
        .sidebar-menu li:hover, .sidebar-menu li.active {
            background-color: rgba(255, 255, 255, 0.1);
            border-left: 4px solid #ffffff;
        }
        .sidebar-menu li i {
            margin-right: 10px;
            width: 20px;
            text-align: center;
        }
        .main-content {
            padding-top: 20px;
            margin-left: 250px;
        }
        .card {
            border-radius: 10px;
            box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.1);
            margin-bottom: 20px;
        }
        .card-header {
            background-color: #f8f9fa;
            border-bottom: 1px solid rgba(0, 0, 0, 0.125);
            font-weight: bold;
        }
        .section-title {
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 20px;
            border-bottom: 2px solid #17a2b8;
            padding-bottom: 10px;
        }
        .btn-primary {
            background-color: #17a2b8;
            border-color: #17a2b8;
        }
        .btn-primary:hover {
            background-color: #138496;
            border-color: #117a8b;
        }
        .nav-tabs .nav-link.active {
            border-color: #dee2e6 #dee2e6 #fff;
            font-weight: bold;
        }
        .dashboard-card {
            border-left: 4px solid;
            border-radius: 4px;
            box-shadow: 0 0.15rem 0.5rem rgba(0, 0, 0, 0.1);
            transition: transform 0.3s;
        }
        .dashboard-card:hover {
            transform: translateY(-5px);
        }
        .status-passed {
            background-color: #d4edda;
            border-color: #28a745;
        }
        .status-failed {
            background-color: #f8d7da;
            border-color: #dc3545;
        }
        .status-pending {
            background-color: #fff3cd;
            border-color: #ffc107;
        }
        .status-skipped {
            background-color: #d1ecf1;
            border-color: #17a2b8;
        }
        /* Page transitions */
        .page {
            display: none;
        }
        .page.active {
            display: block;
            animation: fadeIn 0.5s;
        }
        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }
        .tag-badge {
            margin-right: 5px;
            cursor: pointer;
        }
    </style>
</head>
<body>
    <!-- Sidebar -->
    <div class="sidebar" style="width: 250px;">
        <div class="text-center py-3">
            <h4><i class="fas fa-cube me-2"></i>TestMaster Pro</h4>
        </div>
        <ul class="sidebar-menu">
            <li class="active" data-page="dashboard"><i class="fas fa-tachometer-alt"></i> Dashboard</li>
            <li data-page="test-case-designer"><i class="fas fa-edit"></i> Test Case Designer</li>
            <li data-page="test-data-repo"><i class="fas fa-database"></i> Test Data Repository</li>
            <li data-page="test-runner"><i class="fas fa-play-circle"></i> Test Runner</li>
            <li data-page="expected-results"><i class="fas fa-check-circle"></i> Expected Results</li>
            <li data-page="app-config"><i class="fas fa-cog"></i> App Configuration</li>
        </ul>
    </div>

    <!-- Main Content -->
    <div class="main-content">
        <!-- Dashboard Page -->
        <div class="page active" id="dashboard">
            <div class="container-fluid">
                <h2 class="section-title">Dashboard</h2>
                
                <div class="row mb-4">
                    <div class="col-md-3">
                        <div class="card dashboard-card status-passed">
                            <div class="card-body">
                                <h5 class="card-title">Passed Tests</h5>
                                <h2>127</h2>
                                <p class="text-success">75% <i class="fas fa-arrow-up"></i></p>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="card dashboard-card status-failed">
                            <div class="card-body">
                                <h5 class="card-title">Failed Tests</h5>
                                <h2>24</h2>
                                <p class="text-danger">14% <i class="fas fa-arrow-down"></i></p>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="card dashboard-card status-pending">
                            <div class="card-body">
                                <h5 class="card-title">Pending Tests</h5>
                                <h2>15</h2>
                                <p class="text-warning">9% <i class="fas fa-minus"></i></p>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="card dashboard-card status-skipped">
                            <div class="card-body">
                                <h5 class="card-title">Skipped Tests</h5>
                                <h2>4</h2>
                                <p class="text-info">2% <i class="fas fa-minus"></i></p>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="row">
                    <div class="col-md-6">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-chart-line"></i> Test Execution Trends
                            </div>
                            <div class="card-body">
                                <canvas id="executionTrends" height="250"></canvas>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-exclamation-triangle"></i> Top Failing Test Cases
                            </div>
                            <div class="card-body">
                                <table class="table table-hover">
                                    <thead>
                                        <tr>
                                            <th>Test Case</th>
                                            <th>Failure Rate</th>
                                            <th>Last Run</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        <tr>
                                            <td>TC-001-Authentication-Failed</td>
                                            <td>
                                                <div class="progress">
                                                    <div class="progress-bar bg-danger" style="width: 85%">85%</div>
                                                </div>
                                            </td>
                                            <td>1 hour ago</td>
                                        </tr>
                                        <tr>
                                            <td>TC-015-Payment-Processing</td>
                                            <td>
                                                <div class="progress">
                                                    <div class="progress-bar bg-danger" style="width: 70%">70%</div>
                                                </div>
                                            </td>
                                            <td>3 hours ago</td>
                                        </tr>
                                        <tr>
                                            <td>TC-008-Data-Validation</td>
                                            <td>
                                                <div class="progress">
                                                    <div class="progress-bar bg-danger" style="width: 65%">65%</div>
                                                </div>
                                            </td>
                                            <td>5 hours ago</td>
                                        </tr>
                                        <tr>
                                            <td>TC-032-API-Response</td>
                                            <td>
                                                <div class="progress">
                                                    <div class="progress-bar bg-danger" style="width: 60%">60%</div>
                                                </div>
                                            </td>
                                            <td>Yesterday</td>
                                        </tr>
                                    </tbody>
                                </table>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="row mt-4">
                    <div class="col-md-12">
                        <div class="card">
                            <div class="card-header d-flex justify-content-between align-items-center">
                                <span><i class="fas fa-history"></i> Recent Test Executions</span>
                                <div>
                                    <button class="btn btn-sm btn-outline-primary me-1"><i class="fas fa-download"></i> Export</button>
                                </div>
                            </div>
                            <div class="card-body">
                                <div class="table-responsive">
                                    <div id="recentExecutionsGrid" class="ag-theme-alpine" style="width: 100%; height: 300px;"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Test Case Designer Page -->
        <div class="page" id="test-case-designer">
            <div class="container-fluid">
                <h2 class="section-title">Test Case Designer</h2>
                
                <div class="row mb-3">
                    <div class="col">
                        <div class="card">
                            <div class="card-header d-flex justify-content-between align-items-center">
                                <span><i class="fas fa-edit"></i> Create/Edit Test Cases</span>
                                <div>
                                    <button class="btn btn-primary" id="btnNewTestCase">
                                        <i class="fas fa-plus"></i> New Test Case
                                    </button>
                                </div>
                            </div>
                            <div class="card-body">
                                <div id="testCasesGrid" class="ag-theme-alpine" style="height: 300px;"></div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <!-- Test Case Form -->
                <div class="row">
                    <div class="col">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-file-alt"></i> Test Case Details
                            </div>
                            <div class="card-body">
                                <form id="testCaseForm">
                                    <div class="row mb-3">
                                        <div class="col-md-6">
                                            <div class="mb-3">
                                                <label for="testCaseId" class="form-label">Test Case ID</label>
                                                <input type="text" class="form-control" id="testCaseId" placeholder="TC-001">
                                            </div>
                                            <div class="mb-3">
                                                <label for="testCaseName" class="form-label">Test Case Name</label>
                                                <input type="text" class="form-control" id="testCaseName" placeholder="Login Authentication">
                                            </div>
                                            <div class="mb-3">
                                                <label for="testCaseOwner" class="form-label">Owner</label>
                                                <input type="text" class="form-control" id="testCaseOwner" placeholder="John Doe">
                                            </div>
                                        </div>
                                        <div class="col-md-6">
                                            <div class="mb-3">
                                                <label for="testCaseModule" class="form-label">Module</label>
                                                <select class="form-select" id="testCaseModule">
                                                    <option>Authentication</option>
                                                    <option>User Management</option>
                                                    <option>Payment</option>
                                                    <option>Reporting</option>
                                                </select>
                                            </div>
                                            <div class="mb-3">
                                                <label for="testCasePriority" class="form-label">Priority</label>
                                                <select class="form-select" id="testCasePriority">
                                                    <option>Critical</option>
                                                    <option>High</option>
                                                    <option>Medium</option>
                                                    <option>Low</option>
                                                </select>
                                            </div>
                                            <div class="mb-3">
                                                <label for="testCaseTags" class="form-label">Tags</label>
                                                <input type="text" class="form-control" id="testCaseTags" placeholder="regression, smoke, authentication">
                                            </div>
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3">
                                        <label for="testCaseDescription" class="form-label">Description</label>
                                        <textarea class="form-control" id="testCaseDescription" rows="3" placeholder="Describe the purpose of this test case"></textarea>
                                    </div>
                                    
                                    <div class="mb-3">
                                        <label for="testCasePrerequisites" class="form-label">Prerequisites</label>
                                        <textarea class="form-control" id="testCasePrerequisites" rows="2" placeholder="List any prerequisites for this test case"></textarea>
                                    </div>
                                    
                                    <div class="mb-3">
                                        <label class="form-label">Test Steps</label>
                                        <div class="table-responsive">
                                            <table class="table table-bordered" id="testStepsTable">
                                                <thead>
                                                    <tr>
                                                        <th width="5%">#</th>
                                                        <th width="40%">Action</th>
                                                        <th width="40%">Expected Result</th>
                                                        <th width="15%">Actions</th>
                                                    </tr>
                                                </thead>
                                                <tbody>
                                                    <tr>
                                                        <td>1</td>
                                                        <td><textarea class="form-control" rows="2">Navigate to the login page</textarea></td>
                                                        <td><textarea class="form-control" rows="2">Login page is displayed with username and password fields</textarea></td>
                                                        <td>
                                                            <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td>2</td>
                                                        <td><textarea class="form-control" rows="2">Enter valid username and password</textarea></td>
                                                        <td><textarea class="form-control" rows="2">Input fields accept the entered values</textarea></td>
                                                        <td>
                                                            <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td>3</td>
                                                        <td><textarea class="form-control" rows="2">Click on the Login button</textarea></td>
                                                        <td><textarea class="form-control" rows="2">User is redirected to the dashboard</textarea></td>
                                                        <td>
                                                            <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                                                        </td>
                                                    </tr>
                                                </tbody>
                                            </table>
                                        </div>
                                        <button type="button" class="btn btn-sm btn-outline-primary" id="addTestStepBtn">
                                            <i class="fas fa-plus"></i> Add Step
                                        </button>
                                    </div>
                                    
                                    <div class="d-flex justify-content-between">
                                        <button type="button" class="btn btn-outline-secondary" id="cancelTestCaseBtn">Cancel</button>
                                        <div>
                                            <button type="button" class="btn btn-danger me-2" id="deleteTestCaseBtn">Delete</button>
                                            <button type="button" class="btn btn-primary" id="saveTestCaseBtn">Save Test Case</button>
                                        </div>
                                    </div>
                                </form>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Test Data Repository Page -->
        <div class="page" id="test-data-repo">
            <div class="container-fluid">
                <h2 class="section-title">Test Data Repository</h2>
                
                <div class="row mb-3">
                    <div class="col">
                        <div class="card">
                            <div class="card-header d-flex justify-content-between align-items-center">
                                <span><i class="fas fa-database"></i> Manage Test Data</span>
                                <div>
                                    <button class="btn btn-outline-primary me-2" id="btnGenerateTestData">
                                        <i class="fas fa-robot"></i> Generate with AI
                                    </button>
                                    <button class="btn btn-primary" id="btnUploadTestData">
                                        <i class="fas fa-upload"></i> Upload Test Data
                                    </button>
                                </div>
                            </div>
                            <div class="card-body">
                                <div id="testDataGrid" class="ag-theme-alpine" style="height: 500px;"></div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <!-- AI Test Data Generation Modal -->
                <div class="modal fade" id="generateTestDataModal" tabindex="-1">
                    <div class="modal-dialog modal-lg">
                        <div class="modal-content">
                            <div class="modal-header">
                                <h5 class="modal-title">Generate Test Data with AI</h5>
                                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                            </div>
                            <div class="modal-body">
                                <div class="mb-3">
                                    <label for="testDataNameAI" class="form-label">Test Data Name</label>
                                    <input type="text" class="form-control" id="testDataNameAI" placeholder="User Authentication Data">
                                </div>
                                <div class="mb-3">
                                    <label for="testDataDescription" class="form-label">Description</label>
                                    <textarea class="form-control" id="testDataDescription" rows="2" placeholder="Describe what kind of test data you need"></textarea>
                                </div>
                                <div class="mb-3">
                                    <label for="testDataRequirements" class="form-label">Data Requirements</label>
                                    <textarea class="form-control" id="testDataRequirements" rows="4" placeholder="Specify the structure and requirements for the test data (e.g., 10 users with valid credentials, 5 users with invalid credentials, etc.)"></textarea>
                                </div>
                                <div class="mb-3">
                                    <label for="testDataFormat" class="form-label">Preferred Format</label>
                                    <select class="form-select" id="testDataFormat">
                                        <option value="json">JSON</option>
                                        <option value="csv">CSV</option>
                                        <option value="xml">XML</option>
                                    </select>
                                </div>
                            </div>
                            <div class="modal-footer">
                                <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Cancel</button>
                                <button type="button" class="btn btn-primary" id="btnGenerateAI">Generate Data</button>
                            </div>
                        </div>
                    </div>
                </div>
                
                <!-- Upload Test Data Modal -->
                <div class="modal fade" id="uploadTestDataModal" tabindex="-1">
                    <div class="modal-dialog">
                        <div class="modal-content">
                            <div class="modal-header">
                                <h5 class="modal-title">Upload Test Data</h5>
                                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                            </div>
                            <div class="modal-body">
                                <div class="mb-3">
                                    <label for="testDataName" class="form-label">Test Data Name</label>
                                    <input type="text" class="form-control" id="testDataName" placeholder="User Authentication Data">
                                </div>
                                <div class="mb-3">
                                    <label for="testDataFile" class="form-label">Upload File</label>
                                    <input class="form-control" type="file" id="testDataFile">
                                    <div class="form-text">Supported formats: CSV, JSON, XML, Excel</div>
                                </div>
                                <div class="mb-3">
                                    <label for="testDataTags" class="form-label">Tags</label>
                                    <input type="text" class="form-control" id="testDataTags" placeholder="authentication, users, regression">
                                </div>
                            </div>
                            <div class="modal-footer">
                                <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Cancel</button>
                                <button type="button" class="btn btn-primary" id="btnUploadFile">Upload</button>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Test Runner Page -->
        <div class="page" id="test-runner">
            <div class="container-fluid">
                <h2 class="section-title">Test Runner</h2>
                
                <div class="row mb-3">
                    <div class="col-md-8">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-list-check"></i> Select Test Cases
                            </div>
                            <div class="card-body">
                                <div class="mb-3 d-flex justify-content-between align-items-center">
                                    <div class="input-group" style="max-width: 300px;">
                                        <span class="input-group-text"><i class="fas fa-search"></i></span>
                                        <input type="text" class="form-control" placeholder="Search test cases...">
                                    </div>
                                    <div>
                                        <div class="btn-group">
                                            <button type="button" class="btn btn-outline-secondary" id="selectAllBtn">Select All</button>
                                            <button type="button" class="btn btn-outline-secondary" id="deselectAllBtn">Deselect All</button>
                                        </div>
                                    </div>
                                </div>
                                
                                <!-- Test case selection -->
                                <div id="testCaseSelectionGrid" class="ag-theme-alpine" style="height: 300px;"></div>
                                
                                <!-- Tags filter -->
                                <div class="mt-3">
                                    <label class="fw-bold">Filter by Tags:</label>
                                    <div class="mt-2">
                                        <span class="badge bg-primary tag-badge">regression</span>
                                        <span class="badge bg-primary tag-badge">smoke</span>
                                        <span class="badge bg-primary tag-badge">authentication</span>
                                        <span class="badge bg-primary tag-badge">payment</span>
                                        <span class="badge bg-primary tag-badge">critical</span>
                                        <span class="badge bg-primary tag-badge">api</span>
                                        <span class="badge bg-primary tag-badge">ui</span>
                                    </div>
                                </div>
                                
                                <!-- Group by -->
                                <div class="row mt-3">
                                    <div class="col-md-4">
                                        <label class="fw-bold">Group By:</label>
                                        <select class="form-select mt-1">
                                            <option value="none">None</option>
                                            <option value="module">Module</option>
                                            <option value="priority">Priority</option>
                                            <option value="owner">Owner</option>
                                        </select>
                                    </div>
                                    <div class="col-md-4">
                                        <label class="fw-bold">Test Suite:</label>
                                        <select class="form-select mt-1">
                                            <option value="all">All Test Cases</option>
                                            <option value="regression">Regression Suite</option>
                                            <option value="smoke">Smoke Test Suite</option>
                                            <option value="sanity">Sanity Suite</option>
                                            <option value="custom">Custom Suite</option>
                                        </select>
                                    </div>
                                    <div class="col-md-4">
                                        <label class="fw-bold">Save as Suite:</label>
                                        <div class="input-group mt-1">
                                            <input type="text" class="form-control" placeholder="Suite name">
                                            <button class="btn btn-outline-primary" type="button">Save</button>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    
                    <div class="col-md-4">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-cog"></i> Execution Settings
                            </div>
                            <div class="card-body">
                                <div class="mb-3">
                                    <label for="executionEnvironment" class="form-label">Environment</label>
                                    <select class="form-select" id="executionEnvironment">
                                        <option>Development</option>
                                        <option>Testing</option>
                                        <option>Staging</option>
                                        <option>Production</option>
                                    </select>
                                </div>
                                <div class="mb-3">
                                    <label for="executionBrowser" class="form-label">Browser</label>
                                    <select class="form-select" id="executionBrowser">
                                        <option>Chrome</option>
                                        <option>Firefox</option>
                                        <option>Edge</option>
                                        <option>Safari</option>
                                    </select>
                                </div>
                                <div class="mb-3">
                                    <label for="executionThreads" class="form-label">Parallel Execution Threads</label>
                                    <input type="number" class="form-control" id="executionThreads" value="1" min="1" max="10">
                                </div>
                                <div class="mb-3">
                                    <div class="form-check">
                                        <input class="form-check-input" type="checkbox" id="failFastCheck">
                                        <label class="form-check-label" for="failFastCheck">
                                            Fail Fast (Stop on first failure)
                                        </label>
                                    </div>
                                </div>
                                <div class="mb-3">
                                    <div class="form-check">
                                        <input class="form-check-input" type="checkbox" id="screenshotCheck" checked>
                                        <label class="form-check-label" for="screenshotCheck">
                                            Capture screenshots on failure
                                        </label>
                                    </div>
                                </div>
                                <div class="d-grid gap-2">
                                    <button type="button" class="btn btn-primary" id="runTestsBtn">
                                        <i class="fas fa-play"></i> Run All (25 Selected)
                                    </button>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <!-- Execution Results -->
                <div class="row">
                    <div class="col">
                        <div class="card">
                            <div class="card-header d-flex justify-content-between align-items-center">
                                <span><i class="fas fa-chart-bar"></i> Execution Results</span>
                                <div>
                                    <button class="btn btn-sm btn-outline-primary">
                                        <i class="fas fa-download"></i> Export Report
                                    </button>
                                </div>
                            </div>
                            <div class="card-body">
                                <div class="row mb-3">
                                    <!-- Test Execution Summary -->
                                    <div class="col-md-4">
                                        <div class="card">
                                            <div class="card-body text-center">
                                                <h5>Test Execution Summary</h5>
                                                <div class="d-flex justify-content-around mt-3">
                                                    <div>
                                                        <div class="text-success">
                                                            <i class="fas fa-check-circle fa-2x"></i>
                                                        </div>
                                                        <div>Passed</div>
                                                        <div class="h4">18</div>
                                                    </div>
                                                    <div>
                                                        <div class="text-danger">
                                                            <i class="fas fa-times-circle fa-2x"></i>
                                                        </div>
                                                        <div>Failed</div>
                                                        <div class="h4">5</div>
                                                    </div>
                                                    <div>
                                                        <div class="text-warning">
                                                            <i class="fas fa-exclamation-circle fa-2x"></i>
                                                        </div>
                                                        <div>Skipped</div>
                                                        <div class="h4">2</div>
                                                    </div>
                                                </div>
                                                <div class="progress mt-3">
                                                    <div class="progress-bar bg-success" role="progressbar" style="width: 72%"></div>
                                                    <div class="progress-bar bg-danger" role="progressbar" style="width: 20%"></div>
                                                    <div class="progress-bar bg-warning" role="progressbar" style="width: 8%"></div>
                                                </div>
                                                <div class="mt-3">
                                                    <small class="text-muted">Total Duration: 15m 42s</small>
                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                    
                                    <!-- Execution Time Distribution -->
                                    <div class="col-md-8">
                                        <div class="card">
                                            <div class="card-body">
                                                <h5>Execution Time Distribution</h5>
                                                <canvas id="executionTimeChart" height="180"></canvas>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                                
                                <!-- Detailed Test Results -->
                                <div class="row">
                                    <div class="col-md-12">
                                        <div id="testResultsGrid" class="ag-theme-alpine" style="height: 400px;"></div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- Expected Results Page -->
        <div class="page" id="expected-results">
            <div class="container-fluid">
                <h2 class="section-title">Expected Results</h2>
                
                <div class="row mb-3">
                    <div class="col">
                        <div class="card">
                            <div class="card-header d-flex justify-content-between align-items-center">
                                <span><i class="fas fa-check-circle"></i> Manage Expected Results</span>
                                <div>
                                    <button class="btn btn-outline-primary me-2" id="btnGenerateExpected">
                                        <i class="fas fa-robot"></i> Generate with AI
                                    </button>
                                    <button class="btn btn-primary" id="btnUploadExpected">
                                        <i class="fas fa-upload"></i> Upload Expected Results
                                    </button>
                                </div>
                            </div>
                            <div class="card-body">
                                <div id="expectedResultsGrid" class="ag-theme-alpine" style="height: 500px;"></div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <!-- Expected Results Form -->
                <div class="row">
                    <div class="col">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-file-alt"></i> Expected Result Details
                            </div>
                            <div class="card-body">
                                <form id="expectedResultForm">
                                    <div class="row mb-3">
                                        <div class="col-md-6">
                                            <div class="mb-3">
                                                <label for="ruleCategory" class="form-label">Rule Category</label>
                                                <select class="form-select" id="ruleCategory">
                                                    <option>Data Validation</option>
                                                    <option>Business Rules</option>
                                                    <option>UI/UX</option>
                                                    <option>Security</option>
                                                    <option>Performance</option>
                                                </select>
                                            </div>
                                            <div class="mb-3">
                                                <label for="validationRuleDescription" class="form-label">Validation Rule Description</label>
                                                <textarea class="form-control" id="validationRuleDescription" rows="3" placeholder="Describe the validation rule"></textarea>
                                            </div>
                                        </div>
                                        <div class="col-md-6">
                                            <div class="mb-3">
                                                <label for="expectedResultFile" class="form-label">Expected Result File</label>
                                                <input class="form-control" type="file" id="expectedResultFile">
                                                <div class="form-text">Upload file containing expected output</div>
                                            </div>
                                            <div class="mb-3">
                                                <label for="uploadedBy" class="form-label">Uploaded By</label>
                                                <input type="text" class="form-control" id="uploadedBy" placeholder="John Doe">
                                            </div>
                                        </div>
                                    </div>
                                    
                                    <div class="mb-3">
                                        <label for="expectedResultDescription" class="form-label">Description</label>
                                        <textarea class="form-control" id="expectedResultDescription" rows="3" placeholder="Describe the expected results in detail"></textarea>
                                    </div>
                                    
                                    <div class="mb-3">
                                        <label class="form-label">Associated Test Cases</label>
                                        <select class="form-select" id="associatedTestCases" multiple size="4">
                                            <option>TC-001: Login Authentication</option>
                                            <option>TC-002: User Registration</option>
                                            <option>TC-015: Password Reset</option>
                                            <option>TC-023: Account Lockout</option>
                                        </select>
                                    </div>
                                    
                                    <div class="d-flex justify-content-between">
                                        <button type="button" class="btn btn-outline-secondary" id="cancelExpectedBtn">Cancel</button>
                                        <button type="button" class="btn btn-primary" id="saveExpectedBtn">Save Expected Result</button>
                                    </div>
                                </form>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- App Configuration Page -->
        <div class="page" id="app-config">
            <div class="container-fluid">
                <h2 class="section-title">App Configuration</h2>
                
                <div class="row">
                    <div class="col-md-12">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-cogs"></i> Environment Configuration
                            </div>
                            <div class="card-body">
                                <ul class="nav nav-tabs" id="envTabs" role="tablist">
                                    <li class="nav-item" role="presentation">
                                        <button class="nav-link active" id="dev-tab" data-bs-toggle="tab" data-bs-target="#dev" type="button" role="tab">Development</button>
                                    </li>
                                    <li class="nav-item" role="presentation">
                                        <button class="nav-link" id="test-tab" data-bs-toggle="tab" data-bs-target="#test" type="button" role="tab">Testing</button>
                                    </li>
                                    <li class="nav-item" role="presentation">
                                        <button class="nav-link" id="staging-tab" data-bs-toggle="tab" data-bs-target="#staging" type="button" role="tab">Staging</button>
                                    </li>
                                    <li class="nav-item" role="presentation">
                                        <button class="nav-link" id="prod-tab" data-bs-toggle="tab" data-bs-target="#prod" type="button" role="tab">Production</button>
                                    </li>
                                    <li class="nav-item" role="presentation">
                                        <button class="nav-link" id="new-env-tab" data-bs-toggle="tab" data-bs-target="#new-env" type="button" role="tab">
                                            <i class="fas fa-plus"></i> New
                                        </button>
                                    </li>
                                </ul>
                                <div class="tab-content p-3 border border-top-0 rounded-bottom" id="envTabContent">
                                    <div class="tab-pane fade show active" id="dev" role="tabpanel">
                                        <div class="row">
                                            <div class="col-md-6 mb-3">
                                                <label class="form-label">Base URL</label>
                                                <input type="text" class="form-control" value="https://dev-api.example.com">
                                            </div>
                                            <div class="col-md-6 mb-3">
                                                <label class="form-label">API Version</label>
                                                <input type="text" class="form-control" value="v1">
                                            </div>
                                        </div>
                                        <div class="row">
                                            <div class="col-md-6 mb-3">
                                                <label class="form-label">Database Connection String</label>
                                                <input type="text" class="form-control" value="mongodb://localhost:27017/testdb">
                                            </div>
                                            <div class="col-md-6 mb-3">
                                                <label class="form-label">Authentication Method</label>
                                                <select class="form-select">
                                                    <option selected>OAuth 2.0</option>
                                                    <option>Basic Auth</option>
                                                    <option>API Key</option>
                                                    <option>JWT</option>
                                                </select>
                                            </div>
                                        </div>
                                        <div class="row">
                                            <div class="col-md-6 mb-3">
                                                <label class="form-label">Client ID</label>
                                                <input type="text" class="form-control" value="dev-client-id-123">
                                            </div>
                                            <div class="col-md-6 mb-3">
                                                <label class="form-label">Client Secret</label>
                                                <div class="input-group">
                                                    <input type="password" class="form-control" value="••••••••••••••••">
                                                    <button class="btn btn-outline-secondary" type="button">
                                                        <i class="fas fa-eye"></i>
                                                    </button>
                                                </div>
                                            </div>
                                        </div>
                                        
                                        <h5 class="mt-4">Custom Variables</h5>
                                        <div class="table-responsive">
                                            <table class="table table-bordered" id="envVarsTable">
                                                <thead>
                                                    <tr>
                                                        <th width="30%">Key</th>
                                                        <th width="60%">Value</th>
                                                        <th width="10%">Actions</th>
                                                    </tr>
                                                </thead>
                                                <tbody>
                                                    <tr>
                                                        <td><input type="text" class="form-control" value="TEST_USER"></td>
                                                        <td><input type="text" class="form-control" value="dev_tester@example.com"></td>
                                                        <td>
                                                            <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td><input type="text" class="form-control" value="TEST_PASSWORD"></td>
                                                        <td><input type="password" class="form-control" value="password123"></td>
                                                        <td>
                                                            <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td><input type="text" class="form-control" value="TIMEOUT"></td>
                                                        <td><input type="text" class="form-control" value="5000"></td>
                                                        <td>
                                                            <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                                                        </td>
                                                    </tr>
                                                </tbody>
                                            </table>
                                        </div>
                                        <button type="button" class="btn btn-sm btn-outline-primary" id="addVarBtn">
                                            <i class="fas fa-plus"></i> Add Variable
                                        </button>
                                        
                                        <div class="d-flex justify-content-end mt-4">
                                            <button type="button" class="btn btn-primary" id="saveEnvBtn">Save Configuration</button>
                                        </div>
                                    </div>
                                    <div class="tab-pane fade" id="test" role="tabpanel">
                                        <!-- Testing Environment Configuration (similar structure) -->
                                        <div class="text-center py-5">
                                            <p>Testing environment configuration would have the same fields as Development.</p>
                                        </div>
                                    </div>
                                    <div class="tab-pane fade" id="staging" role="tabpanel">
                                        <!-- Staging Environment Configuration (similar structure) -->
                                        <div class="text-center py-5">
                                            <p>Staging environment configuration would have the same fields as Development.</p>
                                        </div>
                                    </div>
                                    <div class="tab-pane fade" id="prod" role="tabpanel">
                                        <!-- Production Environment Configuration (similar structure) -->
                                        <div class="text-center py-5">
                                            <p>Production environment configuration would have the same fields as Development.</p>
                                        </div>
                                    </div>
                                    <div class="tab-pane fade" id="new-env" role="tabpanel">
                                        <!-- New Environment Form -->
                                        <div class="mb-3">
                                            <label class="form-label">Environment Name</label>
                                            <input type="text" class="form-control" placeholder="Enter environment name">
                                        </div>
                                        <div class="d-flex justify-content-end">
                                            <button type="button" class="btn btn-primary" id="createEnvBtn">Create Environment</button>
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="row mt-4">
                    <div class="col-md-6">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-users-cog"></i> User Management
                            </div>
                            <div class="card-body">
                                <div class="mb-3">
                                    <label class="form-label">Role Management</label>
                                    <div class="table-responsive">
                                        <table class="table table-bordered">
                                            <thead>
                                                <tr>
                                                    <th>Role</th>
                                                    <th>Test Design</th>
                                                    <th>Test Run</th>
                                                    <th>Test Data</th>
                                                    <th>Reports</th>
                                                    <th>Admin</th>
                                                </tr>
                                            </thead>
                                            <tbody>
                                                <tr>
                                                    <td>Admin</td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                </tr>
                                                <tr>
                                                    <td>Test Manager</td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                </tr>
                                                <tr>
                                                    <td>Tester</td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                </tr>
                                                <tr>
                                                    <td>Developer</td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                </tr>
                                                <tr>
                                                    <td>Viewer</td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                    <td class="text-center"><i class="fas fa-check text-success"></i></td>
                                                    <td class="text-center"><i class="fas fa-times text-danger"></i></td>
                                                </tr>
                                            </tbody>
                                        </table>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    
                    <div class="col-md-6">
                        <div class="card">
                            <div class="card-header">
                                <i class="fas fa-bell"></i> Notification Settings
                            </div>
                            <div class="card-body">
                                <div class="mb-3">
                                    <div class="form-check form-switch">
                                        <input class="form-check-input" type="checkbox" id="emailNotifications" checked>
                                        <label class="form-check-label" for="emailNotifications">Email Notifications</label>
                                    </div>
                                </div>
                                <div class="mb-3">
                                    <label for="emailRecipients" class="form-label">Email Recipients</label>
                                    <input type="text" class="form-control" id="emailRecipients" value="team@example.com">
                                    <div class="form-text">Comma-separated list of email addresses</div>
                                </div>
                                <div class="mb-3">
                                    <label class="form-label">Notification Events</label>
                                    <div class="form-check">
                                        <input class="form-check-input" type="checkbox" id="notifyTestCompletion" checked>
                                        <label class="form-check-label" for="notifyTestCompletion">
                                            Test Execution Completion
                                        </label>
                                    </div>
                                    <div class="form-check">
                                        <input class="form-check-input" type="checkbox" id="notifyTestFailure" checked>
                                        <label class="form-check-label" for="notifyTestFailure">
                                            Test Failures
                                        </label>
                                    </div>
                                    <div class="form-check">
                                        <input class="form-check-input" type="checkbox" id="notifyTestCaseCreation">
                                        <label class="form-check-label" for="notifyTestCaseCreation">
                                            New Test Case Creation
                                        </label>
                                    </div>
                                    <div class="form-check">
                                        <input class="form-check-input" type="checkbox" id="notifyTestDataUpdate">
                                        <label class="form-check-label" for="notifyTestDataUpdate">
                                            Test Data Updates
                                        </label>
                                    </div>
                                </div>
                                <div class="d-flex justify-content-end mt-4">
                                    <button type="button" class="btn btn-primary" id="saveNotificationsBtn">Save Settings</button>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>

    <!-- JavaScript Libraries -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap/5.2.3/js/bootstrap.bundle.min.js"></script>
    <!-- Chart.js - Multiple CDN attempt strategy -->
    <script>
        // Function to attempt loading Chart.js from multiple sources
        function loadChartJs() {
            // List of CDNs to try
            const cdnSources = [
                "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/3.9.1/chart.min.js",
                "https://cdn.jsdelivr.net/npm/chart.js@3.9.1/dist/chart.min.js",
                "https://unpkg.com/chart.js@3.9.1/dist/chart.min.js"
            ];
            
            // Try each CDN source until one works
            function tryNextCDN(index) {
                if (index >= cdnSources.length) {
                    console.error("All Chart.js CDN sources failed to load");
                    alert("Unable to load Chart.js library. Charts will not be displayed.");
                    
                    // Add placeholders for charts
                    const chartElements = document.querySelectorAll('canvas[id$="Chart"], #executionTrends');
                    chartElements.forEach(canvas => {
                        const container = canvas.parentElement;
                        const placeholder = document.createElement('div');
                        placeholder.className = 'text-center p-5 bg-light';
                        placeholder.innerHTML = `
                            <p><i class="fas fa-chart-bar fa-3x text-muted mb-3"></i></p>
                            <p class="text-muted">Chart visualization unavailable</p>
                        `;
                        container.replaceChild(placeholder, canvas);
                    });
                    
                    return;
                }
                
                console.log(`Attempting to load Chart.js from: ${cdnSources[index]}`);
                
                const script = document.createElement('script');
                script.src = cdnSources[index];
                script.async = true;
                
                script.onload = function() {
                    console.log(`Chart.js successfully loaded from: ${cdnSources[index]}`);
                    // Initialize the charts
                    window.setTimeout(initializeCharts, 100); // Small delay to ensure Chart is fully initialized
                };
                
                script.onerror = function() {
                    console.warn(`Failed to load Chart.js from: ${cdnSources[index]}`);
                    // Try the next CDN
                    tryNextCDN(index + 1);
                };
                
                document.head.appendChild(script);
            }
            
            // Start with the first CDN
            tryNextCDN(0);
        }
        
        // Call the function when the page is ready
        document.addEventListener('DOMContentLoaded', function() {
            loadChartJs();
        });
    </script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/ag-grid/30.0.2/ag-grid-community.min.js"></script>
    
    <script>
        // Function to initialize charts - will be called once Chart.js is loaded
        function initializeCharts() {
            console.log("Initializing charts...");
            
            // First check if Chart object exists
            if (typeof Chart === 'undefined') {
                console.error("Chart is still undefined when initializing charts");
                return;
            }
            
            try {
                // Setup Chart.js for dashboard charts
                const executionTrendsCtx = document.getElementById('executionTrends');
                if (executionTrendsCtx) {
                    new Chart(executionTrendsCtx, {
                        type: 'line',
                        data: {
                            labels: ['Week 1', 'Week 2', 'Week 3', 'Week 4', 'Week 5', 'Week 6'],
                            datasets: [
                                {
                                    label: 'Passed',
                                    data: [65, 59, 80, 81, 56, 75],
                                    borderColor: '#28a745',
                                    backgroundColor: 'rgba(40, 167, 69, 0.1)',
                                    tension: 0.4
                                },
                                {
                                    label: 'Failed',
                                    data: [28, 48, 40, 19, 26, 27],
                                    borderColor: '#dc3545',
                                    backgroundColor: 'rgba(220, 53, 69, 0.1)',
                                    tension: 0.4
                                },
                                {
                                    label: 'Skipped',
                                    data: [10, 15, 8, 12, 7, 9],
                                    borderColor: '#ffc107',
                                    backgroundColor: 'rgba(255, 193, 7, 0.1)',
                                    tension: 0.4
                                }
                            ]
                        },
                        options: {
                            responsive: true,
                            plugins: {
                                legend: {
                                    position: 'top',
                                },
                                title: {
                                    display: false
                                }
                            },
                            scales: {
                                y: {
                                    beginAtZero: true
                                }
                            }
                        }
                    });
                    console.log("Execution trends chart initialized");
                } else {
                    console.warn("Could not find executionTrends canvas element");
                }
                
                // Setup Chart.js for execution time distribution
                const executionTimeCtx = document.getElementById('executionTimeChart');
                if (executionTimeCtx) {
                    new Chart(executionTimeCtx, {
                        type: 'bar',
                        data: {
                            labels: ['Authentication', 'User Management', 'Payment', 'Orders', 'Products', 'Reporting'],
                            datasets: [
                                {
                                    label: 'Execution Time (seconds)',
                                    data: [12, 8, 15, 10, 7, 11],
                                    backgroundColor: [
                                        'rgba(54, 162, 235, 0.5)',
                                        'rgba(75, 192, 192, 0.5)',
                                        'rgba(255, 159, 64, 0.5)',
                                        'rgba(153, 102, 255, 0.5)',
                                        'rgba(255, 99, 132, 0.5)',
                                        'rgba(255, 205, 86, 0.5)'
                                    ],
                                    borderColor: [
                                        'rgb(54, 162, 235)',
                                        'rgb(75, 192, 192)',
                                        'rgb(255, 159, 64)',
                                        'rgb(153, 102, 255)',
                                        'rgb(255, 99, 132)',
                                        'rgb(255, 205, 86)'
                                    ],
                                    borderWidth: 1
                                }
                            ]
                        },
                        options: {
                            responsive: true,
                            plugins: {
                                legend: {
                                    display: false
                                }
                            },
                            scales: {
                                y: {
                                    beginAtZero: true,
                                    title: {
                                        display: true,
                                        text: 'Execution Time (seconds)'
                                    }
                                }
                            }
                        }
                    });
                    console.log("Execution time chart initialized");
                } else {
                    console.warn("Could not find executionTimeChart canvas element");
                }
            } catch (error) {
                console.error("Error initializing charts:", error);
            }
        }
        
        // Navigation
        document.addEventListener('DOMContentLoaded', function() {
            // Check if libraries loaded
            // Only check for jQuery, Bootstrap and AG Grid
            // Chart.js is loaded separately with its own callback
            if (typeof jQuery === 'undefined' || typeof bootstrap === 'undefined' || typeof agGrid === 'undefined') {
                console.warn("Some core libraries failed to load. Application may not function properly.");
                alert("Some required libraries failed to load. The application may not function correctly.");
            }
            // Sidebar navigation
            const menuItems = document.querySelectorAll('.sidebar-menu li');
            const pages = document.querySelectorAll('.page');
            
            menuItems.forEach(item => {
                item.addEventListener('click', function() {
                    const pageId = this.getAttribute('data-page');
                    
                    // Update active menu item
                    menuItems.forEach(mi => mi.classList.remove('active'));
                    this.classList.add('active');
                    
                    // Show selected page
                    pages.forEach(page => {
                        page.classList.remove('active');
                        if (page.id === pageId) {
                            page.classList.add('active');
                        }
                    });
                });
            });
            
            // Initialize AG-Grid for test cases
            const testCasesGrid = document.getElementById('testCasesGrid');
            if (testCasesGrid) {
                new agGrid.Grid(testCasesGrid, {
                    columnDefs: [
                        { headerName: 'ID', field: 'id', sortable: true, filter: true, width: 100 },
                        { headerName: 'Name', field: 'name', sortable: true, filter: true },
                        { headerName: 'Module', field: 'module', sortable: true, filter: true },
                        { headerName: 'Priority', field: 'priority', sortable: true, filter: true, width: 120 },
                        { headerName: 'Tags', field: 'tags', sortable: true, filter: true },
                        { headerName: 'Status', field: 'status', sortable: true, filter: true, width: 120 }
                    ],
                    rowData: [
                        { id: 'TC-001', name: 'Login Authentication', module: 'Authentication', priority: 'High', tags: 'regression, smoke, authentication', status: 'Ready' },
                        { id: 'TC-002', name: 'User Registration', module: 'Authentication', priority: 'High', tags: 'regression, authentication', status: 'Ready' },
                        { id: 'TC-003', name: 'Password Reset', module: 'Authentication', priority: 'Medium', tags: 'regression, authentication', status: 'Ready' },
                        { id: 'TC-004', name: 'Account Lockout', module: 'Authentication', priority: 'Medium', tags: 'security, authentication', status: 'Ready' },
                        { id: 'TC-005', name: 'User Profile Update', module: 'User Management', priority: 'Low', tags: 'regression, user', status: 'Ready' },
                        { id: 'TC-006', name: 'Payment Processing', module: 'Payment', priority: 'Critical', tags: 'payment, critical', status: 'Ready' },
                        { id: 'TC-007', name: 'Order Creation', module: 'Orders', priority: 'High', tags: 'regression, orders', status: 'Ready' },
                        { id: 'TC-008', name: 'Order Cancellation', module: 'Orders', priority: 'High', tags: 'regression, orders', status: 'Ready' },
                        { id: 'TC-009', name: 'Product Search', module: 'Products', priority: 'Medium', tags: 'regression, products', status: 'Ready' },
                        { id: 'TC-010', name: 'Product Filtering', module: 'Products', priority: 'Medium', tags: 'products', status: 'Ready' }
                    ],
                    defaultColDef: {
                        resizable: true
                    },
                    rowSelection: 'multiple',
                    suppressRowClickSelection: true,
                    domLayout: 'autoHeight'
                });
            }
            
            // Initialize AG-Grid for test results
            const testResultsGrid = document.getElementById('testResultsGrid');
            if (testResultsGrid) {
                new agGrid.Grid(testResultsGrid, {
                    columnDefs: [
                        { headerName: 'Test Case ID', field: 'id', sortable: true, filter: true, width: 120 },
                        { headerName: 'Name', field: 'name', sortable: true, filter: true },
                        { headerName: 'Status', field: 'status', sortable: true, filter: true, width: 120,
                            cellRenderer: function(params) {
                                let badge = '';
                                switch(params.value) {
                                    case 'Passed':
                                        badge = '<span class="badge bg-success">Passed</span>';
                                        break;
                                    case 'Failed':
                                        badge = '<span class="badge bg-danger">Failed</span>';
                                        break;
                                    case 'Skipped':
                                        badge = '<span class="badge bg-warning text-dark">Skipped</span>';
                                        break;
                                    default:
                                        badge = '<span class="badge bg-secondary">Unknown</span>';
                                }
                                return badge;
                            }
                        },
                        { headerName: 'Duration', field: 'duration', sortable: true, filter: true, width: 120 },
                        { headerName: 'Start Time', field: 'startTime', sortable: true, filter: true, width: 180 },
                        { headerName: 'End Time', field: 'endTime', sortable: true, filter: true, width: 180 },
                        {
                            headerName: 'Actions',
                            cellRenderer: function() {
                                return '<button class="btn btn-sm btn-outline-primary me-1"><i class="fas fa-eye"></i> View</button>' +
                                       '<button class="btn btn-sm btn-outline-secondary"><i class="fas fa-redo"></i> Rerun</button>';
                            },
                            width: 150
                        }
                    ],
                    rowData: [
                        { id: 'TC-001', name: 'Login Authentication', status: 'Passed', duration: '2.5s', startTime: '2023-06-15 10:15:22', endTime: '2023-06-15 10:15:25' },
                        { id: 'TC-002', name: 'User Registration', status: 'Passed', duration: '3.1s', startTime: '2023-06-15 10:15:26', endTime: '2023-06-15 10:15:29' },
                        { id: 'TC-003', name: 'Password Reset', status: 'Passed', duration: '2.8s', startTime: '2023-06-15 10:15:30', endTime: '2023-06-15 10:15:33' },
                        { id: 'TC-004', name: 'Account Lockout', status: 'Failed', duration: '4.2s', startTime: '2023-06-15 10:15:34', endTime: '2023-06-15 10:15:38' },
                        { id: 'TC-005', name: 'User Profile Update', status: 'Passed', duration: '2.3s', startTime: '2023-06-15 10:15:39', endTime: '2023-06-15 10:15:41' },
                        { id: 'TC-006', name: 'Payment Processing', status: 'Failed', duration: '5.7s', startTime: '2023-06-15 10:15:42', endTime: '2023-06-15 10:15:48' },
                        { id: 'TC-007', name: 'Order Creation', status: 'Passed', duration: '3.5s', startTime: '2023-06-15 10:15:49', endTime: '2023-06-15 10:15:53' },
                        { id: 'TC-008', name: 'Order Cancellation', status: 'Passed', duration: '2.9s', startTime: '2023-06-15 10:15:54', endTime: '2023-06-15 10:15:57' },
                        { id: 'TC-009', name: 'Product Search', status: 'Skipped', duration: '0.0s', startTime: '2023-06-15 10:15:58', endTime: '2023-06-15 10:15:58' },
                        { id: 'TC-010', name: 'Product Filtering', status: 'Skipped', duration: '0.0s', startTime: '2023-06-15 10:15:59', endTime: '2023-06-15 10:15:59' }
                    ],
                    defaultColDef: {
                        resizable: true
                    },
                    domLayout: 'autoHeight',
                    pagination: true,
                    paginationPageSize: 10
                });
            }
            
            // Initialize AG-Grid for recent executions on dashboard
            const recentExecutionsGrid = document.getElementById('recentExecutionsGrid');
            if (recentExecutionsGrid) {
                new agGrid.Grid(recentExecutionsGrid, {
                    columnDefs: [
                        { headerName: 'Execution ID', field: 'id', sortable: true, filter: true, width: 120 },
                        { headerName: 'Suite', field: 'suite', sortable: true, filter: true },
                        { headerName: 'Environment', field: 'environment', sortable: true, filter: true, width: 120 },
                        { headerName: 'Started By', field: 'startedBy', sortable: true, filter: true },
                        { headerName: 'Start Time', field: 'startTime', sortable: true, filter: true, width: 180 },
                        { headerName: 'Duration', field: 'duration', sortable: true, filter: true, width: 100 },
                        { headerName: 'Status', field: 'status', sortable: true, filter: true, width: 120,
                            cellRenderer: function(params) {
                                let badge = '';
                                switch(params.value) {
                                    case 'Completed':
                                        badge = '<span class="badge bg-success">Completed</span>';
                                        break;
                                    case 'Running':
                                        badge = '<span class="badge bg-primary">Running</span>';
                                        break;
                                    case 'Failed':
                                        badge = '<span class="badge bg-danger">Failed</span>';
                                        break;
                                    case 'Aborted':
                                        badge = '<span class="badge bg-warning text-dark">Aborted</span>';
                                        break;
                                    default:
                                        badge = '<span class="badge bg-secondary">Unknown</span>';
                                }
                                return badge;
                            }
                        },
                        {
                            headerName: 'Actions',
                            cellRenderer: function() {
                                return '<button class="btn btn-sm btn-outline-primary me-1"><i class="fas fa-eye"></i></button>' +
                                       '<button class="btn btn-sm btn-outline-secondary"><i class="fas fa-redo"></i></button>';
                            },
                            width: 120
                        }
                    ],
                    rowData: [
                        { id: 'EX-001', suite: 'Regression Suite', environment: 'Testing', startedBy: 'John Doe', startTime: '2023-06-15 10:15:00', duration: '15m 42s', status: 'Completed' },
                        { id: 'EX-002', suite: 'Smoke Test Suite', environment: 'Development', startedBy: 'Jane Smith', startTime: '2023-06-14 15:30:00', duration: '8m 15s', status: 'Completed' },
                        { id: 'EX-003', suite: 'Payment Module', environment: 'Staging', startedBy: 'Mike Johnson', startTime: '2023-06-14 09:45:00', duration: '12m 38s', status: 'Failed' },
                        { id: 'EX-004', suite: 'User Management', environment: 'Development', startedBy: 'John Doe', startTime: '2023-06-13 14:20:00', duration: '10m 05s', status: 'Completed' },
                        { id: 'EX-005', suite: 'API Tests', environment: 'Testing', startedBy: 'Jane Smith', startTime: '2023-06-13 11:10:00', duration: '11m 52s', status: 'Aborted' }
                    ],
                    defaultColDef: {
                        resizable: true
                    },
                    domLayout: 'autoHeight',
                    pagination: true,
                    paginationPageSize: 5
                });
            }
            
            // Initialize AG-Grid for expected results
            const expectedResultsGrid = document.getElementById('expectedResultsGrid');
            if (expectedResultsGrid) {
                new agGrid.Grid(expectedResultsGrid, {
                    columnDefs: [
                        { headerName: 'ID', field: 'id', sortable: true, filter: true, width: 100 },
                        { headerName: 'Rule Category', field: 'category', sortable: true, filter: true },
                        { headerName: 'Description', field: 'description', sortable: true, filter: true },
                        { headerName: 'Associated Tests', field: 'tests', sortable: true, filter: true },
                        { headerName: 'Uploaded By', field: 'uploadedBy', sortable: true, filter: true },
                        { headerName: 'Last Updated', field: 'lastUpdated', sortable: true, filter: true, width: 150 },
                        {
                            headerName: 'Actions',
                            cellRenderer: function() {
                                return '<button class="btn btn-sm btn-outline-primary me-1"><i class="fas fa-edit"></i></button>' +
                                       '<button class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>';
                            },
                            width: 120
                        }
                    ],
                    rowData: [
                        { id: 'ER-001', category: 'Data Validation', description: 'Username format validation', tests: 'TC-001, TC-002', uploadedBy: 'John Doe', lastUpdated: '2023-06-15' },
                        { id: 'ER-002', category: 'Business Rules', description: 'Payment amount validation', tests: 'TC-006', uploadedBy: 'Jane Smith', lastUpdated: '2023-06-14' },
                        { id: 'ER-003', category: 'UI/UX', description: 'Dashboard layout validation', tests: 'TC-010, TC-011', uploadedBy: 'Mike Johnson', lastUpdated: '2023-06-12' },
                        { id: 'ER-004', category: 'Security', description: 'Password complexity rules', tests: 'TC-003, TC-004', uploadedBy: 'Jane Smith', lastUpdated: '2023-06-10' },
                        { id: 'ER-005', category: 'Performance', description: 'Page load time validation', tests: 'TC-020, TC-021', uploadedBy: 'John Doe', lastUpdated: '2023-06-08' }
                    ],
                    defaultColDef: {
                        resizable: true
                    },
                    domLayout: 'autoHeight',
                    pagination: true,
                    paginationPageSize: 10
                });
            }
            
            // Chart initialization is moved to a separate function called by script onload
            
            // Modal triggers
            $('#btnGenerateTestData').on('click', function() {
                const generateModal = new bootstrap.Modal(document.getElementById('generateTestDataModal'));
                generateModal.show();
            });
            
            $('#btnUploadTestData').on('click', function() {
                const uploadModal = new bootstrap.Modal(document.getElementById('uploadTestDataModal'));
                uploadModal.show();
            });
            
            // Add test step button
            $('#addTestStepBtn').on('click', function() {
                const tbody = document.querySelector('#testStepsTable tbody');
                const rowCount = tbody.rows.length;
                const row = tbody.insertRow();
                
                row.innerHTML = `
                    <td>${rowCount + 1}</td>
                    <td><textarea class="form-control" rows="2"></textarea></td>
                    <td><textarea class="form-control" rows="2"></textarea></td>
                    <td>
                        <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                    </td>
                `;
                
                // Add event listener to delete button
                row.querySelector('.btn-outline-danger').addEventListener('click', function() {
                    row.remove();
                    // Update step numbers
                    const rows = tbody.rows;
                    for (let i = 0; i < rows.length; i++) {
                        rows[i].cells[0].innerText = i + 1;
                    }
                });
            });
            
            // Delete test step buttons
            document.querySelectorAll('#testStepsTable .btn-outline-danger').forEach(button => {
                button.addEventListener('click', function() {
                    const row = this.closest('tr');
                    row.remove();
                    // Update step numbers
                    const rows = document.querySelectorAll('#testStepsTable tbody tr');
                    for (let i = 0; i < rows.length; i++) {
                        rows[i].cells[0].innerText = i + 1;
                    }
                });
            });
            
            // Add environment variable button
            $('#addVarBtn').on('click', function() {
                const tbody = document.querySelector('#envVarsTable tbody');
                const row = tbody.insertRow();
                
                row.innerHTML = `
                    <td><input type="text" class="form-control" value=""></td>
                    <td><input type="text" class="form-control" value=""></td>
                    <td>
                        <button type="button" class="btn btn-sm btn-outline-danger"><i class="fas fa-trash"></i></button>
                    </td>
                `;
                
                // Add event listener to delete button
                row.querySelector('.btn-outline-danger').addEventListener('click', function() {
                    row.remove();
                });
            });
            
            // Delete environment variable buttons
            document.querySelectorAll('#envVarsTable .btn-outline-danger').forEach(button => {
                button.addEventListener('click', function() {
                    const row = this.closest('tr');
                    row.remove();
                });
            });
            
            // Select/Deselect All buttons for test runner
            $('#selectAllBtn').on('click', function() {
                if (testCaseSelectionGrid) {
                    const api = testCaseSelectionGrid.gridOptions.api;
                    api.selectAll();
                }
            });
            
            $('#deselectAllBtn').on('click', function() {
                if (testCaseSelectionGrid) {
                    const api = testCaseSelectionGrid.gridOptions.api;
                    api.deselectAll();
                }
            });
            
            // Tag badges click handler
            $('.tag-badge').on('click', function() {
                $(this).toggleClass('bg-primary bg-secondary');
                // In a real application, this would filter the grid
            });
            
            // Run tests button handler
            $('#runTestsBtn').on('click', function() {
                // Simulate test execution
                alert('Test execution started!');
                // In a real application, this would trigger the test execution
            });
            
            // Form buttons handlers
            $('#saveTestCaseBtn').on('click', function() {
                alert('Test case saved successfully!');
            });
            
            $('#deleteTestCaseBtn').on('click', function() {
                if (confirm('Are you sure you want to delete this test case?')) {
                    alert('Test case deleted successfully!');
                }
            });
            
            $('#cancelTestCaseBtn').on('click', function() {
                if (confirm('Discard changes?')) {
                    // Reset form or navigate away
                }
            });
            
            $('#saveEnvBtn').on('click', function() {
                alert('Environment configuration saved successfully!');
            });
            
            $('#createEnvBtn').on('click', function() {
                alert('New environment created successfully!');
            });
            
            $('#saveNotificationsBtn').on('click', function() {
                alert('Notification settings saved successfully!');
            });
            
            $('#saveExpectedBtn').on('click', function() {
                alert('Expected result saved successfully!');
            });
            
            $('#btnGenerateAI').on('click', function() {
                alert('Generating test data with AI...');
                // Close modal after simulated generation
                setTimeout(function() {
                    bootstrap.Modal.getInstance(document.getElementById('generateTestDataModal')).hide();
                    alert('Test data generated successfully!');
                }, 2000);
            });
            
            $('#btnUploadFile').on('click', function() {
                alert('Uploading test data...');
                // Close modal after simulated upload
                setTimeout(function() {
                    bootstrap.Modal.getInstance(document.getElementById('uploadTestDataModal')).hide();
                    alert('Test data uploaded successfully!');
                }, 1000);
            });
        });
    </script>
</body>
</html>
