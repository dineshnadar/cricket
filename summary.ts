@Component({
  template: `
    <form [formGroup]="employmentForm">
      <div>
        <label>Employment Status</label>
        <select formControlName="employmentStatus">
          <option value="">Select Status</option>
          <option value="Y">Employed</option>
          <option value="N">Unemployed</option>
        </select>
      </div>

      <div>
        <label>Employer Name</label>
        <input formControlName="employerName" placeholder="Employer Name">
      </div>
    </form>
  `
})
export class EmploymentFormComponent implements OnInit {
  employmentForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private ruleFactory: RuleFactoryService,
    private validationService: UnifiedValidationService
  ) {}

  ngOnInit() {
    this.initForm();
    this.setupEmployerNameValidationRule();
  }

  initForm() {
    this.employmentForm = this.fb.group({
      employmentStatus: [''],
      employerName: ['']
    });
  }

  setupEmployerNameValidationRule() {
    // Create a conditional validation rule for Employer Name
    const employerNameRule: FieldRule = this.ruleFactory.createConditionalValidationRule(
      // Validation rules (make it required)
      [{ 
        type: 'required', 
        message: 'Employer Name is required when employed' 
      }],
      
      // Conditions for both visibility and validation
      [{
        field: 'employmentStatus',
        operator: 'equals',
        value: 'Y'
      }],
      
      // Additional options
      {
        runOnLoad: true,
        runOnChange: true,
        runOnDependencyChange: true
      }
    );

    // If you want more explicit control over visibility
    const employerNameVisibilityRule: FieldRule = {
      // Condition for visibility
      conditions: [{
        field: 'employmentStatus',
        operator: 'equals',
        value: 'Y'
      }],
      
      // Visibility configuration
      visibility: {
        value: true,  // Show when condition is met
        clearOnHide: true  // Clear the field when hidden
      },
      
      // Run rules on load and when dependencies change
      runWhen: {
        onLoad: true,
        onChange: true,
        onDependencyChange: true
      }
    };

    // Register the rules with the validation service
    this.validationService.registerRules(this.employmentForm.value, {
      'employerName': this.ruleFactory.combineRules(
        employerNameRule, 
        employerNameVisibilityRule
      )
    });
  }
}
