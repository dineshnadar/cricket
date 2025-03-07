  <script>
        $(document).ready(function() {
            // Sample test case data
            const testCases = [
                { id: 'TC-001', name: 'Login Authentication', module: 'Authentication', priority: 'High', owner: 'John Doe', status: 'Ready', lastUpdated: '2023-06-15' },
                { id: 'TC-002', name: 'User Registration', module: 'Authentication', priority: 'High', owner: 'Jane Smith', status: 'Ready', lastUpdated: '2023-06-14' },
                { id: 'TC-003', name: 'Password Reset', module: 'Authentication', priority: 'Medium', owner: 'John Doe', status: 'Ready', lastUpdated: '2023-06-12' },
                { id: 'TC-004', name: 'Account Lockout', module: 'Authentication', priority: 'Medium', owner: 'Jane Smith', status: 'InProgress', lastUpdated: '2023-06-10' },
                { id: 'TC-005', name: 'User Profile Update', module: 'User Management', priority: 'Low', owner: 'John Doe', status: 'Ready', lastUpdated: '2023-06-08' },
                { id: 'TC-006', name: 'Payment Processing', module: 'Payment', priority: 'Critical', owner: 'Mike Johnson', status: 'Ready', lastUpdated: '2023-06-07' },
                { id: 'TC-007', name: 'Order Creation', module: 'Orders', priority: 'High', owner: 'Jane Smith', status: 'InReview', lastUpdated: '2023-06-05' },
                { id: 'TC-008', name: 'Order Cancellation', module: 'Orders', priority: 'High', owner: 'Mike Johnson', status: 'Ready', lastUpdated: '2023-06-04' },
                { id: 'TC-009', name: 'Product Search', module: 'Products', priority: 'Medium', owner: 'John Doe', status: 'Draft', lastUpdated: '2023-06-03' },
                { id: 'TC-010', name: 'Product Filtering', module: 'Products', priority: 'Medium', owner: 'Jane Smith', status: 'Ready', lastUpdated: '2023-06-02' }
            ];

            let currentGroupBy = 'module';
            let searchTerm = '';
            
            // Initialize the table
            renderTable();
            
            // Group by dropdown change event
            $('#groupBySelect').on('change', function() {
                currentGroupBy = $(this).val();
                renderTable();
            });
            
            // Search input keyup event
            $('#searchInput').on('keyup', function() {
                searchTerm = $(this).val().toLowerCase();
                renderTable();
            });
            
            // Toggle group expand/collapse
            $(document).on('click', '.group-header', function() {
                $(this).toggleClass('expanded');
                
                // Update the chevron icon
                const icon = $(this).find('i.fas');
                if ($(this).hasClass('expanded')) {
                    icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
                } else {
                    icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
                }
            });
            
            // Select all checkbox
            $('#selectAllCheckbox').on('change', function() {
                const isChecked = $(this).prop('checked');
                $('.row-checkbox:visible').prop('checked', isChecked);
                updateSelectionCounter();
            });
            
            // Individual row checkbox
            $(document).on('change', '.row-checkbox', function() {
                updateSelectionCounter();
                
                // Check if all visible checkboxes are checked
                const allChecked = $('.row-checkbox:visible').length === $('.row-checkbox:visible:checked').length;
                $('#selectAllCheckbox').prop('checked', allChecked);
            });
            
            // Run selected button
            $('#runSelectedBtn').on('click', function() {
                const selectedIds = [];
                $('.row-checkbox:checked').each(function() {
                    selectedIds.push($(this).data('id'));
                });
                
                if (selectedIds.length > 0) {
                    alert('Running tests: ' + selectedIds.join(', '));
                } else {
                    alert('Please select at least one test case to run.');
                }
            });
            
            // Function to render the table
            function renderTable() {
                const tableBody = $('#tableBody');
                tableBody.empty();
                
                // Filter data based on search term
                const filteredData = testCases.filter(testCase => 
                    Object.values(testCase).some(value => 
                        value.toString().toLowerCase().includes(searchTerm)
                    )
                );
                
                // Group the data
                const groups = {};
                filteredData.forEach(testCase => {
                    const groupValue = testCase[currentGroupBy];
                    if (!groups[groupValue]) {
                        groups[groupValue] = [];
                    }
                    groups[groupValue].push(testCase);
                });
                
                // Build table HTML
                $.each(groups, function(groupName, items) {
                    // Group header row
                    const groupHeaderRow = $('<tr>').addClass('group-header fw-bold')
                        .append(
                            $('<td>').attr('colspan', 7).addClass('px-4 py-2')
                                .append(
                                    $('<div>').addClass('d-flex align-items-center')
                                        .append(
                                            $('<i>').addClass('fas fa-chevron-right me-2')
                                        )
                                        .append(
                                            $('<span>').text(
                                                capitalizeFirstLetter(currentGroupBy) + ': ' + 
                                                groupName + ' (' + items.length + ')'
                                            )
                                        )
                                )
                        );
                    
                    tableBody.append(groupHeaderRow);
                    
                    // Data rows for the group
                    $.each(items, function(index, testCase) {
                        const dataRow = $('<tr>').addClass('table-row')
                            .append(
                                $('<td>').addClass('px-4 py-3')
                                    .append(
                                        $('<div>').addClass('form-check')
                                            .append(
                                                $('<input>').addClass('form-check-input row-checkbox')
                                                    .attr('type', 'checkbox')
                                                    .data('id', testCase.id)
                                            )
                                    )
                            )
                            .append($('<td>').addClass('px-4 py-3').text(testCase.id))
                            .append($('<td>').addClass('px-4 py-3').text(testCase.name))
                            .append(
                                $('<td>').addClass('px-4 py-3')
                                    .append(
                                        $('<span>').addClass('badge badge-priority-' + testCase.priority)
                                            .text(testCase.priority)
                                    )
                            )
                            .append(
                                $('<td>').addClass('px-4 py-3')
                                    .append(
                                        $('<span>').addClass('badge badge-status-' + testCase.status)
                                            .text(testCase.status.replace(/([A-Z])/g, ' $1').trim()) // Add spaces before capital letters
                                    )
                            )
                            .append($('<td>').addClass('px-4 py-3').text(testCase.owner))
                            .append($('<td>').addClass('px-4 py-3').text(testCase.lastUpdated));
                        
                        tableBody.append(dataRow);
                    });
                });
                
                // No results message
                if (filteredData.length === 0) {
                    tableBody.append(
                        $('<tr>').append(
                            $('<td>').attr('colspan', 7).addClass('text-center py-4')
                                .text('No test cases found matching your search criteria')
                        )
                    );
                }
                
                // Reset checkboxes and counters
                $('#selectAllCheckbox').prop('checked', false);
                updateSelectionCounter();
            }
            
            // Helper function to update selection counter
            function updateSelectionCounter() {
                const selectedCount = $('.row-checkbox:checked').length;
                const totalVisibleCount = $('.row-checkbox:visible').length;
                $('#selectionCounter').text(selectedCount + ' of ' + totalVisibleCount + ' test cases selected');
            }
            
            // Helper function to capitalize first letter
            function capitalizeFirstLetter(string) {
                return string.charAt(0).toUpperCase() + string.slice(1);
            }
        });
    </script>
