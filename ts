private compareObjects(
  initial: Record<string, any> | null, 
  current: Record<string, any> | null,
  path: string,
  control?: FormGroup
): Record<string, any> {
  // Early returns and validations
  if (!control) {
    return {};
  }

  if (this.formIgnoreService.shouldIgnoreControl(control, path)) {
    return {};
  }

  const changes: Record<string, any> = {};
  let objectStatus: ChangeStatus = 'unmodified';

  // Handle null/undefined initial case
  if (!initial) {
    // Use form structure to track changes instead of recursion
    const leafControls = this.getLeafControls(control, path);
    
    leafControls.forEach(({ control: ctrl, path: ctrlPath }) => {
      const change = {
        label: this.getControlLabel(ctrl, ctrlPath),
        formControlName: ctrlPath,
        path: ctrlPath,
        old: null,
        new: ctrl.value,
        status: 'added',
        sectionName: this.getSectionName(path)
      };

      changes[ctrlPath] = change;
      objectStatus = 'modified';
    });

    return changes;
  }

  // For existing values, use Map for better performance
  const initialKeys = new Set(Object.keys(initial));
  const currentKeys = new Set(Object.keys(current || {}));
  const allKeys = new Set([...initialKeys, ...currentKeys]);

  // Process each key
  for (const key of allKeys) {
    const propertyPath = path ? `${path}.${key}` : key;
    const propertyControl = control.get(key);

    // Skip if control should be ignored
    if (!propertyControl || this.formIgnoreService.shouldIgnoreControl(propertyControl, propertyPath)) {
      continue;
    }

    // Determine change type
    if (!currentKeys.has(key)) {
      changes[key] = this.createChange(
        propertyPath,
        propertyControl,
        initial[key],
        undefined,
        'deleted'
      );
      objectStatus = 'modified';
    }
    else if (!initialKeys.has(key)) {
      changes[key] = this.createChange(
        propertyPath,
        propertyControl,
        undefined,
        current[key],
        'added'
      );
      objectStatus = 'modified';
    }
    else {
      const valueChanges = this.compareValues(
        initial[key],
        current[key],
        propertyPath,
        propertyControl
      );

      if (valueChanges && Object.keys(valueChanges).length > 0) {
        changes[key] = valueChanges;
        objectStatus = 'modified';
      }
    }
  }

  return Object.keys(changes).length > 0 ? { ...changes, status: objectStatus } : {};
}

// Helper method to get leaf controls (actual form controls, not groups/arrays)
private getLeafControls(control: AbstractControl, basePath: string = ''): Array<{ control: FormControl, path: string }> {
  const controls: Array<{ control: FormControl, path: string }> = [];

  const processControl = (ctrl: AbstractControl, currentPath: string) => {
    if (ctrl instanceof FormControl) {
      controls.push({ control: ctrl, path: currentPath });
    }
    else if (ctrl instanceof FormGroup) {
      Object.entries(ctrl.controls).forEach(([key, childCtrl]) => {
        const childPath = currentPath ? `${currentPath}.${key}` : key;
        processControl(childCtrl, childPath);
      });
    }
    else if (ctrl instanceof FormArray) {
      ctrl.controls.forEach((childCtrl, index) => {
        processControl(childCtrl, `${currentPath}[${index}]`);
      });
    }
  };

  processControl(control, basePath);
  return controls;
}

// Helper method to create change object
private createChange(
  path: string,
  control: AbstractControl,
  oldValue: any,
  newValue: any,
  status: ChangeStatus
): any {
  return {
    label: this.getControlLabel(control, path),
    formControlName: path,
    path,
    old: oldValue,
    new: newValue,
    status,
    sectionName: this.getSectionName(path)
  };
}

// Helper method to compare values
private compareValues(
  oldValue: any,
  newValue: any,
  path: string,
  control: AbstractControl
): any {
  if (this.areValuesEqual(oldValue, newValue)) {
    return null;
  }

  if (control instanceof FormGroup) {
    return this.compareObjects(oldValue, newValue, path, control);
  }

  if (control instanceof FormArray) {
    return this.compareArrays(oldValue, newValue, path, control);
  }

  return this.createChange(path, control, oldValue, newValue, 'modified');
}




------------------x---------
private compareArrays(
  initial: any[], 
  current: any[], 
  path: string,
  control?: FormArray
): Record<string, any> {
  const changes: Record<string, any> = {};
  let arrayStatus: ChangeStatus = 'unmodified';

  // Handle null/empty initial case
  if (!initial || initial.length === 0) {
    // Process each item in current array
    current?.forEach((value, index) => {
      const itemPath = `${path}[${index}]`;
      const arrayControl = control?.at(index);

      if (arrayControl instanceof FormGroup) {
        // Handle nested form group
        const nestedChanges = this.compareObjects(
          null,
          value,
          itemPath,
          arrayControl
        );
        if (Object.keys(nestedChanges).length > 0) {
          changes[index] = nestedChanges;
          arrayStatus = 'modified';
        }
      } else if (arrayControl instanceof FormArray) {
        // Handle nested form array
        const nestedChanges = this.compareArrays(
          null,
          value,
          itemPath,
          arrayControl
        );
        if (Object.keys(nestedChanges).length > 0) {
          changes[index] = nestedChanges;
          arrayStatus = 'modified';
        }
      } else {
        // Handle simple values
        changes[index] = this.createArrayItemChange(
          itemPath,
          arrayControl,
          null,
          value,
          'added'
        );
        arrayStatus = 'modified';
      }
    });

    if (Object.keys(changes).length > 0) {
      changes.status = arrayStatus;
    }
    return changes;
  }

  // Rest of the existing comparison logic for when initial values exist
  const maxLength = Math.max(initial.length, current.length);

  for (let i = 0; i < maxLength; i++) {
    const itemPath = `${path}[${i}]`;
    const itemControl = control?.at(i);

    if (i >= current.length) {
      changes[i] = this.createArrayItemChange(
        itemPath,
        itemControl,
        initial[i],
        undefined,
        'deleted'
      );
      arrayStatus = 'modified';
    } else if (i >= initial.length) {
      changes[i] = this.createArrayItemChange(
        itemPath,
        itemControl,
        undefined,
        current[i],
        'added'
      );
      arrayStatus = 'modified';
    } else {
      const itemChanges = this.compareStates(
        initial[i],
        current[i],
        itemPath,
        itemControl
      );
      
      if (Object.keys(itemChanges).length > 0) {
        changes[i] = itemChanges;
        arrayStatus = 'modified';
      }
    }
  }

  if (Object.keys(changes).length > 0) {
    changes.status = arrayStatus;
  }

  return changes;
}


private compareObjects(
  initial: Record<string, any>, 
  current: Record<string, any>,
  path: string,
  control?: FormGroup
): Record<string, any> {
  const changes: Record<string, any> = {};
  let objectStatus: ChangeStatus = 'unmodified';

  // Handle null/empty initial case
  if (!initial || Object.keys(initial).length === 0) {
    // Process each field in current object
    Object.keys(current || {}).forEach(key => {
      const propertyPath = path ? `${path}.${key}` : key;
      const propertyControl = control?.get(key);
      const currentValue = current[key];

      if (propertyControl instanceof FormGroup) {
        // For nested objects
        const nestedChanges = this.compareObjects(
          null,
          currentValue,
          propertyPath,
          propertyControl
        );
        if (Object.keys(nestedChanges).length > 0) {
          changes[key] = nestedChanges;
          objectStatus = 'modified';
        }
      } else {
        // For leaf nodes (form controls)
        changes[key] = this.createObjectPropertyChange(
          propertyPath,
          propertyControl,
          null,
          currentValue,
          'added'
        );
        objectStatus = 'modified';
      }
    });

    if (Object.keys(changes).length > 0) {
      changes.status = objectStatus;
    }

    return changes;
  }

  // Normal object comparison for existing values
  const allKeys = new Set([
    ...Object.keys(initial || {}),
    ...Object.keys(current || {})
  ]);

  for (const key of allKeys) {
    const propertyPath = path ? `${path}.${key}` : key;
    const propertyControl = control?.get(key);

    if (!(key in current)) {
      changes[key] = this.createObjectPropertyChange(
        propertyPath,
        propertyControl,
        initial[key],
        undefined,
        'deleted'
      );
      objectStatus = 'modified';
    } else if (!(key in initial)) {
      changes[key] = this.createObjectPropertyChange(
        propertyPath,
        propertyControl,
        undefined,
        current[key],
        'added'
      );
      objectStatus = 'modified';
    } else {
      const propertyChanges = this.compareStates(
        initial[key],
        current[key],
        propertyPath,
        propertyControl
      );

      if (Object.keys(propertyChanges).length > 0) {
        changes[key] = propertyChanges;
        objectStatus = 'modified';
      }
    }
  }

  if (Object.keys(changes).length > 0) {
    changes.status = objectStatus;
  }

  return changes;
}
