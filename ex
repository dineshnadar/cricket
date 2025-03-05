// unified-validation.service.ts
import { Injectable, Inject, Optional, InjectionToken } from '@angular/core';
import { AbstractControl, FormGroup, FormArray, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { FormExtensionService } from './form-extension.service';
import { BehaviorSubject, Observable, Subject, Subscription } from 'rxjs';

/**
 * Common interfaces and types
 */
export interface LookupItem {
  label: string;
  value: any;
  [key: string]: any;
}

export type FieldOperator = 
  'equals' | 
  'notEquals' | 
  'contains' | 
  'in' | 
  'greaterThan' | 
  'lessThan' | 
  'isEmpty' | 
  'isNotEmpty' | 
  'startsWith' | 
  'endsWith' | 
  'regex' | 
  'between';

export interface ValidationRule {
  type: 'required' | 'email' | 'minLength' | 'maxLength' | 'pattern' | 'custom';
  value?: any;
  message?: string;
  customValidatorName?: string;
  params?: any;
}

/**
 * Cross-form field reference
 */
export interface FieldReference {
  formId?: string;  // Optional - defaults to current form if not provided
  field: string;
}

/**
 * Condition for field rules
 */
export interface Condition {
  field: string | string[] | FieldReference | FieldReference[];
  operator: FieldOperator;
  value?: any;
  negate?: boolean;  // Negate the condition result
}

/**
 * Lookup rule configuration
 */
export interface LookupRule {
  // Either specify a lookup name to pull from LookupService
  lookupName?: string;
  
  // Or apply filtering to existing lookupData in FormExtension
  filterCurrentLookup?: boolean;
  
  // Field to filter by
  filterBy?: {
    field: string | FieldReference;
    property?: string; // Property on lookup item to filter by (default: 'value')
  };
  
  // Custom transformation function
  transformLookup?: (
    lookupData: any[], 
    forms: Record<string, FormGroup>,
    context?: any
  ) => any[];
}

/**
 * Field rule configuration
 */
export interface FieldRule {
  // Conditions that apply to all rule types
  conditions?: Condition[];
  
  // Visibility configuration
  visibility?: {
    value: boolean;
    clearOnHide?: boolean;
  };
  
  // Disable configuration
  disable?: {
    value: boolean;
    clearOnDisable?: boolean;
    conditions?: Condition[]; // Specific conditions for disable
  };
  
  // Validation configuration
  validation?: {
    conditions?: Condition[];
    rules: ValidationRule[];
  };
  
  // Lookup configuration
  lookup?: LookupRule;
  
  // Execution timing flags
  runWhen?: {
    onLoad?: boolean;
    onChange?: boolean;
    onDependencyChange?: boolean;
    manual?: boolean;
  };
}

/**
 * Field value change event
 */
export interface FieldChangeEvent {
  formId: string;
  path: string;
  value: any;
  previousValue?: any;
}

/**
 * Validation controller interface
 */
export interface ValidationController {
  runAllRules(timing?: RuleTiming): void;
  runRulesForField(formId: string, fieldPath: string): void;
  updateLookup(formId: string, fieldPath: string, options: { 
    lookupName?: string; 
    filterField?: string | FieldReference;
  }): void;
  validateForms(): boolean;
  markAllAsTouched(): void;
  getValidationErrors(): Record<string, ValidationErrors>;
  destroy(): void;
}

// Type for rule timing
export type RuleTiming = 'onLoad' | 'onChange' | 'onDependencyChange' | 'manual';

/**
 * Custom validator function interface
 */
export interface CustomValidatorFn {
  (params?: any): ValidatorFn;
}

/**
 * InjectionToken for custom validators
 */
export const CUSTOM_VALIDATORS = new InjectionToken<Record<string, CustomValidatorFn>>(
  'CUSTOM_VALIDATORS'
);

/**
 * InjectionToken for lookup service
 */
export const LOOKUP_SERVICE = new InjectionToken<LookupService>(
  'LOOKUP_SERVICE'
);

/**
 * Interface for lookup service
 */
export interface LookupService {
  getLookupData(lookupName: string, params?: any): LookupItem[];
}

/**
 * Unified validation service combining single and cross-form validation
 */
@Injectable({ providedIn: 'root' })
export class UnifiedValidationService {
  private formRegistry = new Map<string, FormGroup>();
  private rulesRegistry = new Map<string, Record<string, FieldRule>>();
  private fieldChangeSubject = new Subject<FieldChangeEvent>();
  private subscriptions = new Map<string, Subscription[]>();
  private valueCache = new Map<string, Map<string, any>>();
  private formValueChangeSubscriptions = new Map<string, Subscription>();
  private originalValidators = new Map<string, Map<string, ValidatorFn[]>>();
  
  /**
   * Get field change events as Observable
   */
  public fieldChanges: Observable<FieldChangeEvent> = this.fieldChangeSubject.asObservable();
  
  constructor(
    private formExtension: FormExtensionService,
    @Optional() @Inject(LOOKUP_SERVICE) private lookupService: LookupService,
    @Optional() @Inject(CUSTOM_VALIDATORS) private customValidators: Record<string, CustomValidatorFn>
  ) {
    // Initialize with empty objects if not provided
    this.customValidators = this.customValidators || {};
  }
  
  /**
   * Register a form with the validation service
   */
  registerForm(formId: string, formGroup: FormGroup): void {
    // Unregister if already exists
    this.unregisterForm(formId);
    
    // Add to registry
    this.formRegistry.set(formId, formGroup);
    this.valueCache.set(formId, new Map<string, any>());
    this.originalValidators.set(formId, new Map<string, ValidatorFn[]>());
    
    // Store original validators for all controls
    this.storeOriginalValidators(formId, formGroup);
    
    // Subscribe to form value changes
    const subscription = formGroup.valueChanges.subscribe(values => {
      this.processFormChanges(formId, formGroup, values);
    });
    
    this.formValueChangeSubscriptions.set(formId, subscription);
  }
  
  /**
   * Store original validators for all controls in a form
   */
  private storeOriginalValidators(formId: string, formGroup: FormGroup, parentPath: string = ''): void {
    const validatorsMap = this.originalValidators.get(formId);
    
    Object.entries(formGroup.controls).forEach(([key, control]) => {
      const path = parentPath ? `${parentPath}.${key}` : key;
      
      if (control instanceof FormGroup) {
        this.storeOriginalValidators(formId, control, path);
      } else if (control instanceof FormArray) {
        // Store FormArray validators
        if (control.validator) {
          validatorsMap.set(path, Array.isArray(control.validator) ? control.validator : [control.validator]);
        }
        
        // Process FormArray items
        control.controls.forEach((item, index) => {
          if (item instanceof FormGroup) {
            this.storeOriginalValidators(formId, item, `${path}[${index}]`);
          } else {
            const itemPath = `${path}[${index}]`;
            if (item.validator) {
              validatorsMap.set(itemPath, Array.isArray(item.validator) ? item.validator : [item.validator]);
            }
          }
        });
      } else {
        // Store control validators
        if (control.validator) {
          validatorsMap.set(path, Array.isArray(control.validator) ? control.validator : [control.validator]);
        }
      }
    });
  }
  
  /**
   * Unregister a form from the validation service
   */
  unregisterForm(formId: string): void {
    // Clean up subscriptions
    if (this.formValueChangeSubscriptions.has(formId)) {
      this.formValueChangeSubscriptions.get(formId)?.unsubscribe();
      this.formValueChangeSubscriptions.delete(formId);
    }
    
    const subscriptions = this.subscriptions.get(formId) || [];
    subscriptions.forEach(sub => sub.unsubscribe());
    this.subscriptions.delete(formId);
    
    // Remove from registries
    this.formRegistry.delete(formId);
    this.rulesRegistry.delete(formId);
    this.valueCache.delete(formId);
    this.originalValidators.delete(formId);
  }
  
  /**
   * Process form value changes to detect field changes
   */
  private processFormChanges(
    formId: string, 
    formGroup: FormGroup, 
    newValues: any, 
    parentPath: string = ''
  ): void {
    const valuesCache = this.valueCache.get(formId);
    if (!valuesCache) return;
    
    // Helper function to flatten object paths
    const flattenObject = (obj: any, parentPath = ''): Record<string, any> => {
      const result: Record<string, any> = {};
      
      Object.entries(obj).forEach(([key, value]) => {
        const path = parentPath ? `${parentPath}.${key}` : key;
        
        if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
          Object.assign(result, flattenObject(value, path));
        } else {
          result[path] = value;
        }
      });
      
      return result;
    };
    
    // Flatten object to detect individual field changes
    const flattenedValues = flattenObject(newValues, parentPath);
    
    Object.entries(flattenedValues).forEach(([path, newValue]) => {
      const oldValue = valuesCache.get(path);
      
      // Check if value actually changed (avoid circular updates)
      if (JSON.stringify(oldValue) !== JSON.stringify(newValue)) {
        // Update cache
        valuesCache.set(path, newValue);
        
        // Emit change event
        this.fieldChangeSubject.next({
          formId,
          path,
          value: newValue,
          previousValue: oldValue
        });
        
        // Get control for the path
        const control = formGroup.get(path);
        
        // Find and run onChange rules for this field
        if (control) {
          const rules = this.rulesRegistry.get(formId);
          const fieldRule = rules?.[path];
          
          if (fieldRule?.runWhen?.onChange) {
            this.evaluateFieldRules(formId, path, fieldRule, 'onChange');
          }
        }
        
        // Run dependency rules
        this.runDependencyRules(formId, path, newValue);
      }
    });
  }
  
  /**
   * Run rules that depend on a changed field
   */
  private runDependencyRules(formId: string, changedPath: string, newValue: any): void {
    // Check all forms and rules for dependencies
    this.rulesRegistry.forEach((rules, ruleFormId) => {
      Object.entries(rules).forEach(([fieldPath, rule]) => {
        // Skip if not configured to run on dependency change
        if (!rule.runWhen?.onDependencyChange) return;
        
        // Check if any conditions reference the changed field
        const hasDependency = this.doesRuleHaveDependency(rule, formId, changedPath);
        
        if (hasDependency) {
          this.evaluateFieldRules(ruleFormId, fieldPath, rule, 'onDependencyChange');
        }
      });
    });
  }
  
  /**
   * Check if a rule depends on a specific field
   */
  private doesRuleHaveDependency(
    rule: FieldRule, 
    dependencyFormId: string, 
    dependencyPath: string
  ): boolean {
    // Check main conditions
    if (this.conditionsHaveDependency(rule.conditions, dependencyFormId, dependencyPath)) {
      return true;
    }
    
    // Check disable conditions
    if (rule.disable?.conditions && 
        this.conditionsHaveDependency(rule.disable.conditions, dependencyFormId, dependencyPath)) {
      return true;
    }
    
    // Check validation conditions
    if (rule.validation?.conditions && 
        this.conditionsHaveDependency(rule.validation.conditions, dependencyFormId, dependencyPath)) {
      return true;
    }
    
    // Check lookup filterBy
    if (rule.lookup?.filterBy) {
      const filterField = rule.lookup.filterBy.field;
      
      if (typeof filterField === 'string') {
        // Same-form field
        // This can't be a dependency from another form
        return false;
      } else {
        // Cross-form reference
        return (
          (!filterField.formId || filterField.formId === dependencyFormId) && 
          filterField.field === dependencyPath
        );
      }
    }
    
    return false;
  }
  
  /**
   * Check if conditions reference a specific field
   */
  private conditionsHaveDependency(
    conditions: Condition[] | undefined, 
    dependencyFormId: string, 
    dependencyPath: string
  ): boolean {
    if (!conditions) return false;
    
    return conditions.some(condition => {
      if (Array.isArray(condition.field)) {
        // Check array of fields
        return condition.field.some(field => {
          if (typeof field === 'string') {
            // Same-form field
            // This can't be a dependency from another form
            return false;
          } else {
            // Cross-form reference
            return (
              (!field.formId || field.formId === dependencyFormId) && 
              field.field === dependencyPath
            );
          }
        });
      } else if (typeof condition.field === 'string') {
        // Same-form field
        // This can't be a dependency from another form
        return false;
      } else if (condition.field) {
        // Cross-form reference
        return (
          (!condition.field.formId || condition.field.formId === dependencyFormId) && 
          condition.field.field === dependencyPath
        );
      }
      
      return false;
    });
  }
  
  /**
   * Register validation rules for a form
   */
  registerRules(
    formId: string,
    rules: Record<string, FieldRule>
  ): ValidationController {
    // Get existing rules
    const existingRules = this.rulesRegistry.get(formId) || {};
    
    // Merge with new rules
    const mergedRules = { ...existingRules, ...rules };
    
    // Store merged rules
    this.rulesRegistry.set(formId, mergedRules);
    
    // Set up form field subscriptions
    const formGroup = this.formRegistry.get(formId);
    if (formGroup) {
      // Only setup extensions for new rules
      this.setupFieldExtensions(formId, formGroup, rules);
    }
    
    // Return controller
    return this.createController();
  }
  
  /**
   * Set up form field extensions
   */
  private setupFieldExtensions(
    formId: string,
    formGroup: FormGroup,
    rules: Record<string, FieldRule>
  ): void {
    // Process each rule
    Object.entries(rules).forEach(([fieldPath, rule]) => {
      const control = formGroup.get(fieldPath);
      if (!control) return;
      
      // Initialize form extension properties
      this.formExtension.extendControl(control, {
        visible: true,
        disabled: false,
        required: false
      });
    });
  }
  
  /**
   * Evaluate rules for a specific field
   */
  private evaluateFieldRules(
    formId: string,
    fieldPath: string,
    rule: FieldRule,
    timing: RuleTiming = 'manual'
  ): void {
    const formGroup = this.formRegistry.get(formId);
    if (!formGroup) return;
    
    const control = formGroup.get(fieldPath);
    if (!control) return;
    
    // Check if this rule should run at the specified timing
    if (timing !== 'manual') {
      const shouldRun = rule.runWhen?.[timing];
      if (!shouldRun) return;
    }
    
    // Evaluate shared conditions
    const conditionsMet = !rule.conditions || 
      this.evaluateConditions(formId, rule.conditions);
    
    // Apply visibility rules
    if (rule.visibility !== undefined) {
      const isVisible = conditionsMet && rule.visibility.value;
      this.formExtension.updateControlProperty(control, 'visible', isVisible);
      
      // Clear value if hidden
      if (!isVisible && rule.visibility.clearOnHide) {
        this.clearControlValue(control);
      }
    }
    
    // Apply disable rules
    if (rule.disable !== undefined) {
      // Check disable-specific conditions if any
      const disableConditionsMet = !rule.disable.conditions || 
        this.evaluateConditions(formId, rule.disable.conditions);
      
      const isDisabled = conditionsMet && disableConditionsMet && rule.disable.value;
      this.formExtension.updateControlProperty(control, 'disabled', isDisabled);
      
      // Set the native disabled state
      if (isDisabled) {
        control.disable({ emitEvent: false });
        
        // Clear value if needed
        if (rule.disable.clearOnDisable) {
          this.clearControlValue(control);
        }
      } else {
        control.enable({ emitEvent: false });
      }
    }
    
    // Apply validation rules
    if (rule.validation) {
      // Check validation-specific conditions if any
      const validationConditionsMet = !rule.validation.conditions || 
        this.evaluateConditions(formId, rule.validation.conditions);
      
      // Apply validation if all conditions are met
      if (conditionsMet && validationConditionsMet) {
        // Create validators from rules
        const ruleValidators = this.createValidators(rule.validation.rules);
        
        // Combine with original validators
        const originalValidators = this.getOriginalValidators(formId, fieldPath);
        const combinedValidators = [...originalValidators, ...ruleValidators];
        
        // Apply validators
        control.setValidators(combinedValidators);
        
        // Mark control as required in the FormExtension if any validator is 'required'
        const isRequired = ruleValidators.some(v => 
          this.isRequiredValidator(v)
        );
        
        this.formExtension.updateControlProperty(control, 'required', isRequired);
      } else {
        // Reset to original validators
        const originalValidators = this.getOriginalValidators(formId, fieldPath);
        control.setValidators(originalValidators.length > 0 ? originalValidators : null);
        
        // Remove required flag
        this.formExtension.updateControlProperty(control, 'required', false);
      }
      
      control.updateValueAndValidity({ emitEvent: false });
    }
    
    // Apply lookup rules
    if (rule.lookup) {
      this.applyLookupRules(formId, fieldPath, control, rule.lookup, conditionsMet);
    }
  }
  
  /**
   * Check if a validator is a required validator
   */
  private isRequiredValidator(validator: ValidatorFn): boolean {
    // First check for direct equality with the standard required validators
    if (validator === Validators.required || validator === Validators.requiredTrue) {
      return true;
    }
    
    // Try to identify required validators by testing with a null input
    const testControl = { value: null } as any;
    const errors = validator(testControl);
    return errors && ('required' in errors);
  }
  
  /**
   * Get original validators for a control
   */
  private getOriginalValidators(formId: string, fieldPath: string): ValidatorFn[] {
    const validatorsMap = this.originalValidators.get(formId);
    if (!validatorsMap) return [];
    
    return validatorsMap.get(fieldPath) || [];
  }
  
  /**
   * Evaluate conditions
   */
  private evaluateConditions(
    formId: string,
    conditions: Condition[]
  ): boolean {
    return conditions.every(condition => {
      // Multiple fields
      if (Array.isArray(condition.field)) {
        return condition.field.every(field => {
          const { targetForm, fieldPath } = this.resolveFieldReference(formId, field);
          if (!targetForm) return false;
          
          const control = targetForm.get(fieldPath);
          if (!control) return false;
          
          const result = this.evaluateCondition(control.value, condition.operator, condition.value);
          return condition.negate ? !result : result;
        });
      } 
      // Single field
      else {
        const { targetForm, fieldPath } = this.resolveFieldReference(formId, condition.field);
        if (!targetForm) return false;
        
        const control = targetForm.get(fieldPath);
        if (!control) return false;
        
        const result = this.evaluateCondition(control.value, condition.operator, condition.value);
        return condition.negate ? !result : result;
      }
    });
  }
  
  /**
   * Resolve a field reference to a form and path
   */
  private resolveFieldReference(
    currentFormId: string,
    reference: string | FieldReference
  ): { targetForm: FormGroup | null, fieldPath: string } {
    // String reference = field in current form
    if (typeof reference === 'string') {
      return {
        targetForm: this.formRegistry.get(currentFormId),
        fieldPath: reference
      };
    }
    // FieldReference = potentially cross-form reference
    else {
      const targetFormId = reference.formId || currentFormId;
      return {
        targetForm: this.formRegistry.get(targetFormId),
        fieldPath: reference.field
      };
    }
  }
  
  /**
   * Evaluate a single condition
   */
  private evaluateCondition(
    value: any,
    operator: FieldOperator,
    compareValue?: any
  ): boolean {
    switch (operator) {
      case 'equals':
        return value === compareValue;
      case 'notEquals':
        return value !== compareValue;
      case 'contains':
        return Array.isArray(value) ? 
          value.includes(compareValue) : 
          String(value).includes(String(compareValue));
      case 'in':
        return Array.isArray(compareValue) ? compareValue.includes(value) : false;
      case 'greaterThan':
        return value > compareValue;
      case 'lessThan':
        return value < compareValue;
      case 'isEmpty':
        return value === null || value === '' || value === undefined || 
          (Array.isArray(value) && value.length === 0);
      case 'isNotEmpty':
        return value !== null && value !== '' && value !== undefined && 
          (!Array.isArray(value) || value.length > 0);
      case 'startsWith':
        return String(value).startsWith(String(compareValue));
      case 'endsWith':
        return String(value).endsWith(String(compareValue));
      case 'regex':
        return new RegExp(compareValue).test(value);
      case 'between':
        return Array.isArray(compareValue) && compareValue.length === 2 
          ? value >= compareValue[0] && value <= compareValue[1]
          : false;
      default:
        return false;
    }
  }
  
  /**
   * Apply lookup rules to a control
   */
  private applyLookupRules(
    formId: string,
    fieldPath: string,
    control: AbstractControl,
    lookupRule: LookupRule,
    conditionsMet: boolean
  ): void {
    if (!conditionsMet) return;
    
    let lookupData: LookupItem[] = [];
    
    // Get lookup data from the appropriate source
    if (lookupRule.filterCurrentLookup) {
      // Get existing lookupData from control
      const existingLookupData = this.formExtension.getExtendedProperties(control).lookupData;
      if (Array.isArray(existingLookupData)) {
        lookupData = existingLookupData;
      }
    } else if (lookupRule.lookupName && this.lookupService) {
      // Get lookup data from lookup service
      lookupData = this.lookupService.getLookupData(lookupRule.lookupName);
    }
    
    // Apply filter based on another field's value
    if (lookupRule.filterBy) {
      const { targetForm, fieldPath: filterFieldPath } = 
        this.resolveFieldReference(formId, lookupRule.filterBy.field);
      
      if (targetForm) {
        const filterControl = targetForm.get(filterFieldPath);
        if (filterControl) {
          const filterValue = filterControl.value;
          const property = lookupRule.filterBy.property || 'value';
          
          lookupData = lookupData.filter(item => {
            // Handle different lookup data structures
            if (typeof item === 'object' && item !== null) {
              // Check item properties or nested metadata
              if (property in item) {
                return item[property] === filterValue;
              } else if (item.metadata && property in item.metadata) {
                return item.metadata[property] === filterValue;
              }
            }
            return false;
          });
        }
      }
    }
    
    // Apply custom transformation if provided
    if (lookupRule.transformLookup) {
      const formsMap: Record<string, FormGroup> = {};
      this.formRegistry.forEach((form, id) => {
        formsMap[id] = form;
      });
      
      lookupData = lookupRule.transformLookup(lookupData, formsMap) || [];
    }
    
    // Update the form extension property
    this.formExtension.updateControlProperty(control, 'lookupData', lookupData);
    
    // Reset invalid value if necessary
    this.resetInvalidLookupValue(control, lookupData);
  }
  
  /**
   * Reset control value if it's not in the lookup data
   */
  private resetInvalidLookupValue(control: AbstractControl, lookupData: LookupItem[]): void {
    const currentValue = control.value;
    
    // Don't reset if value is empty
    if (currentValue === null || currentValue === undefined || currentValue === '') {
      return;
    }
    
    // Check if value exists in lookup data
    const valueExists = lookupData.some(item => {
      if (typeof item === 'object' && item !== null) {
        return item.value === currentValue || item.id === currentValue;
      } else {
        return item === currentValue;
      }
    });
    
    // Reset value if not found
    if (!valueExists) {
      control.setValue(null);
      control.markAsDirty();
    }
  }
  
  /**
   * Create validators from validation rules
   */
  private createValidators(rules: ValidationRule[]): ValidatorFn[] {
    return rules.map(rule => {
      switch (rule.type) {
        case 'required':
          return Validators.required;
        case 'email':
          return Validators.email;
        case 'minLength':
          return Validators.minLength(rule.value);
        case 'maxLength':
          return Validators.maxLength(rule.value);
        case 'pattern':
          return Validators.pattern(rule.value);
        case 'custom':
          if (rule.customValidatorName && this.customValidators[rule.customValidatorName]) {
            return this.customValidators[rule.customValidatorName](rule.params);
          }
          return () => null; // No-op validator if custom validator not found
        default:
          return () => null;
      }
    });
  }
  
  /**
   * Clear a control's value recursively
   */
  private clearControlValue(control: AbstractControl): void {
    if (control instanceof FormGroup) {
      Object.values(control.controls).forEach(childControl => {
        this.clearControlValue(childControl);
      });
    } else if (control instanceof FormArray) {
      control.clear();
    } else {
      control.reset();
      control.markAsDirty();
    }
  }
  
  /**
   * Create validation controller
   */
  private createController(): ValidationController {
    return {
      runAllRules: (timing?: RuleTiming) => 
        this.runAllRules(timing),
      runRulesForField: (formId: string, fieldPath: string) => 
        this.runRulesForField(formId, fieldPath),
      updateLookup: (formId: string, fieldPath: string, options) => 
        this.updateLookup(formId, fieldPath, options),
      validateForms: () => this.validateForms(),
      markAllAsTouched: () => this.markAllAsTouched(),
      getValidationErrors: () => this.getValidationErrors(),
      destroy: () => this.destroyController()
    };
  }
  
  /**
   * Run all rules
   */
  private runAllRules(
    timing: RuleTiming = 'manual'
  ): void {
    this.rulesRegistry.forEach((rules, formId) => {
      Object.entries(rules).forEach(([fieldPath, rule]) => {
        this.evaluateFieldRules(formId, fieldPath, rule, timing);
      });
    });
  }
  
  /**
   * Run rules for a specific field
   */
  private runRulesForField(formId: string, fieldPath: string): void {
    const rules = this.rulesRegistry.get(formId);
    if (!rules) return;
    
    const rule = rules[fieldPath];
    if (rule) {
      this.evaluateFieldRules(formId, fieldPath, rule, 'manual');
    }
  }
  
  /**
   * Update a field's lookup configuration
   */
  private updateLookup(
    formId: string,
    fieldPath: string,
    options: { 
      lookupName?: string; 
      filterField?: string | FieldReference;
    }
  ): void {
    const rules = this.rulesRegistry.get(formId);
    if (!rules) return;
    
    const rule = rules[fieldPath];
    if (!rule) return;
    
    // Update the rule
    const updatedRule: FieldRule = { ...rule };
    
    // Create or update lookup config
    if (!updatedRule.lookup) {
      updatedRule.lookup = {};
    }
    
    // Update lookupName if provided
    if (options.lookupName) {
      updatedRule.lookup.lookupName = options.lookupName;
      updatedRule.lookup.filterCurrentLookup = false;
    }
    
    // Update filterBy field if provided
    if (options.filterField) {
      if (!updatedRule.lookup.filterBy) {
        updatedRule.lookup.filterBy = { field: options.filterField };
      } else {
        updatedRule.lookup.filterBy.field = options.filterField;
      }
    }
    
    // Update the registry
    rules[fieldPath] = updatedRule;
    
    // Run the updated rule
    this.evaluateFieldRules(formId, fieldPath, updatedRule, 'manual');
  }
  
  /**
   * Validate all registered forms
   */
  private validateForms(): boolean {
    let allValid = true;
    
    // Mark all forms as touched and check validity
    this.formRegistry.forEach(formGroup => {
      this.markFormGroupTouched(formGroup);
      
      if (!formGroup.valid) {
        allValid = false;
      }
    });
    
    return allValid;
  }
  
  /**
   * Mark all controls in all forms as touched
   */
  private markAllAsTouched(): void {
    this.formRegistry.forEach(formGroup => {
      this.markFormGroupTouched(formGroup);
    });
  }
  
  /**
   * Mark all controls in a form as touched
   */
  private markFormGroupTouched(formGroup: FormGroup): void {
    Object.values(formGroup.controls).forEach(control => {
      if (control instanceof FormGroup) {
        this.markFormGroupTouched(control);
      } else if (control instanceof FormArray) {
        for (let i = 0; i < control.length; i++) {
          const arrayControl = control.at(i);
          if (arrayControl instanceof FormGroup) {
            this.markFormGroupTouched(arrayControl);
          } else {
            arrayControl.markAsTouched();
          }
        }
      } else {
        control.markAsTouched();
      }
    });
  }
  
  /**
   * Get all validation errors from all forms
   */
  private getValidationErrors(): Record<string, ValidationErrors> {
    const errors: Record<string, ValidationErrors> = {};
    
    this.formRegistry.forEach((formGroup, formId) => {
      const formErrors = this.getFormErrors(formGroup);
      if (Object.keys(formErrors).length > 0) {
        errors[formId] = formErrors;
      }
    });
    
    return errors;
  }
  
  /**
   * Get all validation errors from a form
   */
  private getFormErrors(formGroup: FormGroup, path: string = ''): ValidationErrors {
    const errors: ValidationErrors = {};
    
    Object.entries(formGroup.controls).forEach(([key, control]) => {
      const controlPath = path ? `${path}.${key}` : key;
      
      if (control instanceof FormGroup) {
        const nestedErrors = this.getFormErrors(control, controlPath);
        if (Object.keys(nestedErrors).length > 0) {
          errors[controlPath] = nestedErrors;
        }
      } else if (control instanceof FormArray) {
        const arrayErrors: ValidationErrors[] = [];
        
        for (let i = 0; i < control.length; i++) {
          const arrayControl = control.at(i);
          if (arrayControl instanceof FormGroup) {
            const nestedErrors = this.getFormErrors(arrayControl, `${controlPath}[${i}]`);
            if (Object.keys(nestedErrors).length > 0) {
              arrayErrors.push(nestedErrors);
            }
          } else if (arrayControl.errors) {
            arrayErrors.push(arrayControl.errors);
          }
        }
        
        if (arrayErrors.length > 0) {
          errors[controlPath] = arrayErrors;
        }
      } else if (control.errors) {
        errors[controlPath] = control.errors;
      }
    });
    
    return errors;
  }
  
  /**
   * Clean up all subscriptions when controller is destroyed
   */
  private destroyController(): void {
    // Clean up all form subscriptions
    this.formValueChangeSubscriptions.forEach(sub => sub.unsubscribe());
    this.formValueChangeSubscriptions.clear();
    
    // Clean up field subscriptions
    this.subscriptions.forEach(subs => {
      subs.forEach(sub => sub.unsubscribe());
    });
    this.subscriptions.clear();
    
    // Clear registries
    this.formRegistry.clear();
    this.rulesRegistry.clear();
    this.valueCache.clear();
    this.originalValidators.clear();
  }
  
  /**
   * Get a form by its ID - useful for cross-profile validation
   */
  getForm(formId: string): FormGroup | null {
    return this.formRegistry.get(formId) || null;
  }
  
  /**
   * Get all forms - useful for finding all profiles
   */
  getAllForms(): Map<string, FormGroup> {
    return this.formRegistry;
  }
  
  /**
   * Update a field's validation rules at runtime
   */
  updateValidationRules(
    formId: string,
    fieldPath: string,
    newValidationRules: ValidationRule[]
  ): void {
    // Get current rules
    const rules = this.rulesRegistry.get(formId);
    if (!rules || !rules[fieldPath]) return;
    
    // Create updated rule
    const updatedRule = { ...rules[fieldPath] };
    
    // Update validation rules
    if (!updatedRule.validation) {
      updatedRule.validation = { rules: newValidationRules };
    } else {
      updatedRule.validation.rules = newValidationRules;
    }
    
    // Update rule registry
    rules[fieldPath] = updatedRule;
    
    // Re-run the rule
    this.runRulesForField(formId, fieldPath);
  }
  
  /**
   * Update a field's visibility rule at runtime
   */
  updateVisibilityRule(
    formId: string,
    fieldPath: string,
    isVisible: boolean,
    clearOnHide?: boolean
  ): void {
    // Get current rules
    const rules = this.rulesRegistry.get(formId);
    if (!rules || !rules[fieldPath]) return;
    
    // Create updated rule
    const updatedRule = { ...rules[fieldPath] };
    
    // Update visibility
    if (!updatedRule.visibility) {
      updatedRule.visibility = { 
        value: isVisible,
        clearOnHide: clearOnHide ?? false
      };
    } else {
      updatedRule.visibility.value = isVisible;
      if (clearOnHide !== undefined) {
        updatedRule.visibility.clearOnHide = clearOnHide;
      }
    }
    
    // Update rule registry
    rules[fieldPath] = updatedRule;
    
    // Re-run the rule
    this.runRulesForField(formId, fieldPath);
  }
  
  /**
   * Update a field's disable rule at runtime
   */
  updateDisableRule(
    formId: string,
    fieldPath: string,
    isDisabled: boolean,
    clearOnDisable?: boolean
  ): void {
    // Get current rules
    const rules = this.rulesRegistry.get(formId);
    if (!rules || !rules[fieldPath]) return;
    
    // Create updated rule
    const updatedRule = { ...rules[fieldPath] };
    
    // Update disable rule
    if (!updatedRule.disable) {
      updatedRule.disable = { 
        value: isDisabled,
        clearOnDisable: clearOnDisable ?? false
      };
    } else {
      updatedRule.disable.value = isDisabled;
      if (clearOnDisable !== undefined) {
        updatedRule.disable.clearOnDisable = clearOnDisable;
      }
    }
    
    // Update rule registry
    rules[fieldPath] = updatedRule;
    
    // Re-run the rule
    this.runRulesForField(formId, fieldPath);
  }
  
  /**
   * Update a field's conditions at runtime
   */
  updateFieldConditions(
    formId: string,
    fieldPath: string,
    newConditions: Condition[],
    applyTo: 'all' | 'visibility' | 'validation' | 'disable' = 'all'
  ): void {
    // Get current rules
    const rules = this.rulesRegistry.get(formId);
    if (!rules || !rules[fieldPath]) return;
    
    // Create updated rule
    const updatedRule = { ...rules[fieldPath] };
    
    // Update conditions based on target
    if (applyTo === 'all' || applyTo === 'visibility') {
      updatedRule.conditions = [...newConditions];
    }
    
    if (applyTo === 'all' || applyTo === 'validation') {
      if (!updatedRule.validation) {
        updatedRule.validation = { 
          conditions: [...newConditions],
          rules: [] 
        };
      } else {
        updatedRule.validation.conditions = [...newConditions];
      }
    }
    
    if (applyTo === 'all' || applyTo === 'disable') {
      if (!updatedRule.disable) {
        updatedRule.disable = { 
          conditions: [...newConditions],
          value: true
        };
      } else {
        updatedRule.disable.conditions = [...newConditions];
      }
    }
    
    // Update rule registry
    rules[fieldPath] = updatedRule;
    
    // Re-run the rule
    this.runRulesForField(formId, fieldPath);
  }
  
  /**
   * Add FormArray handling for dynamically adding form elements
   */
  registerArrayItem(
    parentFormId: string,
    arrayPath: string,
    itemIndex: number,
    rulesTemplate?: Record<string, FieldRule>
  ): string {
    // Get parent form
    const parentForm = this.formRegistry.get(parentFormId);
    if (!parentForm) {
      throw new Error(`Parent form ${parentFormId} not found`);
    }
    
    // Get array control
    const arrayControl = parentForm.get(arrayPath);
    if (!arrayControl || !(arrayControl instanceof FormArray)) {
      throw new Error(`${arrayPath} is not a FormArray`);
    }
    
    // Get item at index
    const itemControl = arrayControl.at(itemIndex);
    if (!itemControl || !(itemControl instanceof FormGroup)) {
      throw new Error(`Item at index ${itemIndex} is not a FormGroup`);
    }
    
    // Create item ID
    const itemFormId = `${parentFormId}.${arrayPath}[${itemIndex}]`;
    
    // Register the item form
    this.registerForm(itemFormId, itemControl as FormGroup);
    
    // Apply rules template if provided
    if (rulesTemplate) {
      this.registerRules(itemFormId, rulesTemplate);
    }
    
    return itemFormId;
  }
  
  /**
   * Unregister a form array item
   */
  unregisterArrayItem(
    parentFormId: string,
    arrayPath: string,
    itemIndex: number
  ): void {
    const itemFormId = `${parentFormId}.${arrayPath}[${itemIndex}]`;
    this.unregisterForm(itemFormId);
  }
  
  /**
   * Re-index array items after removal
   */
  reindexArrayItems(
    parentFormId: string,
    arrayPath: string,
    startIndex: number
  ): void {
    // Get parent form
    const parentForm = this.formRegistry.get(parentFormId);
    if (!parentForm) return;
    
    // Get array control
    const arrayControl = parentForm.get(arrayPath);
    if (!arrayControl || !(arrayControl instanceof FormArray)) return;
    
    // For each item from startIndex, update the registration
    for (let i = startIndex; i < arrayControl.length; i++) {
      const oldFormId = `${parentFormId}.${arrayPath}[${i+1}]`;
      const newFormId = `${parentFormId}.${arrayPath}[${i}]`;
      
      // Get the form for this item
      const itemForm = this.formRegistry.get(oldFormId);
      if (itemForm) {
        // Get the rules for this item
        const rules = this.rulesRegistry.get(oldFormId);
        
        // Unregister old ID
        this.unregisterForm(oldFormId);
        
        // Register with new ID
        this.registerForm(newFormId, itemForm);
        
        // Register rules if they existed
        if (rules) {
          this.registerRules(newFormId, rules);
        }
      }
    }
  }
}
