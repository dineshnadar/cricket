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
    // Use strict equality for primitive values and deep comparison for objects
    const valueChanged = typeof newValue !== 'object' || newValue === null 
      ? oldValue !== newValue 
      : JSON.stringify(oldValue) !== JSON.stringify(newValue);
      
    if (valueChanged) {
      // Update cache with new value
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
      
      // Always run dependency rules for the changed field
      this.runDependencyRules(formId, path, newValue);
    }
  });
}


private runDependencyRules(formId: string, changedPath: string, newValue: any): void {
  // Check all forms and rules for dependencies
  this.rulesRegistry.forEach((rules, ruleFormId) => {
    Object.entries(rules).forEach(([fieldPath, rule]) => {
      // Skip if not configured to run on dependency change
      if (!rule.runWhen?.onDependencyChange) {
        return;
      }
      
      // Check if any conditions reference the changed field
      const hasDependency = this.doesRuleHaveDependency(rule, formId, changedPath);
      
      if (hasDependency) {
        // Execute the rule for the dependent field
        this.evaluateFieldRules(ruleFormId, fieldPath, rule, 'onDependencyChange');
      }
    });
  });
}

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
      // Same-form field within the same form
      return dependencyPath === filterField;
    } else if (filterField) {
      // Cross-form reference
      return (
        (!filterField.formId || filterField.formId === dependencyFormId) && 
        filterField.field === dependencyPath
      );
    }
  }
  
  return false;
}

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
          // Same-form field - THIS IS THE CRITICAL FIX
          return field === dependencyPath;
        } else {
          // Cross-form reference
          return (
            (!field.formId || field.formId === dependencyFormId) && 
            field.field === dependencyPath
          );
        }
      });
    } else if (typeof condition.field === 'string') {
      // Same-form field - THIS IS THE CRITICAL FIX
      return condition.field === dependencyPath;
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
