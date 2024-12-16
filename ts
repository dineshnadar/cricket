private compareObjects(
  initial: Record<string, any> | null, 
  current: Record<string, any> | null,
  path: string,
  control?: FormGroup
): Record<string, any> {
  const changes: Record<string, any> = {};

  if (!control) return {};

  // For null/empty initial case, get control level changes
  if (!initial) {
    // Use recursion to get to the control level
    const getControlChanges = (ctrl: AbstractControl, currentPath: string) => {
      if (ctrl instanceof FormControl) {
        // For leaf nodes (actual form controls)
        changes[currentPath] = {
          label: this.getControlLabel(ctrl, currentPath),
          formControlName: currentPath,
          path: currentPath,
          old: null,
          new: ctrl.value,
          status: 'added',
          sectionName: this.getSectionName(currentPath)
        };
      } 
      else if (ctrl instanceof FormGroup) {
        // Recurse through FormGroup
        Object.keys(ctrl.controls).forEach(key => {
          const nextPath = currentPath ? `${currentPath}.${key}` : key;
          getControlChanges(ctrl.get(key), nextPath);
        });
      } 
      else if (ctrl instanceof FormArray) {
        // Recurse through FormArray
        ctrl.controls.forEach((arrayCtrl, index) => {
          if (arrayCtrl instanceof FormGroup) {
            // If array contains FormGroup, recurse through its controls
            Object.keys(arrayCtrl.controls).forEach(key => {
              const nextPath = `${currentPath}[${index}].${key}`;
              getControlChanges(arrayCtrl.get(key), nextPath);
            });
          } else {
            // For simple array controls
            const nextPath = `${currentPath}[${index}]`;
            getControlChanges(arrayCtrl, nextPath);
          }
        });
      }
    };

    getControlChanges(control, path);
    return changes;
  }

  // For existing values
  Object.keys(control.controls).forEach(key => {
    const childControl = control.get(key);
    const childPath = path ? `${path}.${key}` : key;

    if (childControl instanceof FormControl) {
      if (!this.areValuesEqual(initial[key], current?.[key])) {
        changes[key] = {
          label: this.getControlLabel(childControl, childPath),
          formControlName: childPath,
          path: childPath,
          old: initial[key],
          new: current?.[key],
          status: this.getChangeStatus(initial[key], current?.[key]),
          sectionName: this.getSectionName(childPath)
        };
      }
    } 
    else if (childControl instanceof FormGroup) {
      const groupChanges = this.compareObjects(
        initial[key],
        current?.[key],
        childPath,
        childControl
      );
      if (Object.keys(groupChanges).length > 0) {
        Object.assign(changes, groupChanges);
      }
    }
    else if (childControl instanceof FormArray) {
      // Handle array within form group
      const arrayChanges = {};
      const oldArray = Array.isArray(initial[key]) ? initial[key] : [];
      const newArray = Array.isArray(current?.[key]) ? current[key] : [];
      
      childControl.controls.forEach((arrayCtrl, index) => {
        if (arrayCtrl instanceof FormGroup) {
          // For FormGroup within array
          Object.keys(arrayCtrl.controls).forEach(groupKey => {
            const fieldControl = arrayCtrl.get(groupKey);
            const fieldPath = `${childPath}[${index}].${groupKey}`;
            
            arrayChanges[fieldPath] = {
              label: this.getControlLabel(fieldControl, fieldPath),
              formControlName: fieldPath,
              path: fieldPath,
              old: oldArray[index]?.[groupKey],
              new: newArray[index]?.[groupKey],
              status: this.getChangeStatus(
                oldArray[index]?.[groupKey],
                newArray[index]?.[groupKey]
              ),
              sectionName: this.getSectionName(childPath)
            };
          });
        } else {
          // For simple controls in array
          const arrayPath = `${childPath}[${index}]`;
          arrayChanges[arrayPath] = {
            label: this.getControlLabel(arrayCtrl, arrayPath),
            formControlName: arrayPath,
            path: arrayPath,
            old: oldArray[index],
            new: newArray[index],
            status: this.getChangeStatus(oldArray[index], newArray[index]),
            sectionName: this.getSectionName(childPath)
          };
        }
      });
      
      Object.assign(changes, arrayChanges);
    }
  });

  return changes;
}

private getChangeStatus(oldValue: any, newValue: any): ChangeStatus {
  if (oldValue === undefined || oldValue === null) return 'added';
  if (newValue === undefined || newValue === null) return 'deleted';
  return 'modified';
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
