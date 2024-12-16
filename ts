private compareObjects(
  initial: Record<string, any> | null, 
  current: Record<string, any> | null,
  path: string,
  control?: FormGroup
): Record<string, any> {
  const changes: Record<string, any> = {};

  // Early return if no control
  if (!control) return {};

  // When initial is null, get individual field changes
  if (!initial) {
    Object.keys(control.controls).forEach(key => {
      const childControl = control.get(key);
      const childPath = path ? `${path}.${key}` : key;

      if (childControl instanceof FormControl) {
        // Handle simple control
        changes[key] = {
          label: this.getControlLabel(childControl, childPath),
          formControlName: childPath,
          path: childPath,
          old: null,
          new: childControl.value,
          status: 'added',
          sectionName: this.getSectionName(path)
        };
      } 
      else if (childControl instanceof FormArray) {
        // Handle form array
        const arrayChanges = this.compareArrays(
          null, 
          childControl.value,
          childPath,
          childControl
        );
        if (Object.keys(arrayChanges).length > 0) {
          Object.assign(changes, arrayChanges);
        }
      }
      else if (childControl instanceof FormGroup) {
        // Handle nested form group
        const groupChanges = this.compareObjects(
          null,
          childControl.value,
          childPath,
          childControl
        );
        if (Object.keys(groupChanges).length > 0) {
          Object.assign(changes, groupChanges);
        }
      }
    });

    return changes;
  }

  // Normal comparison for non-null case
  const allKeys = new Set([...Object.keys(initial), ...Object.keys(current || {})]);

  allKeys.forEach(key => {
    const childPath = path ? `${path}.${key}` : key;
    const childControl = control.get(key);

    if (!(key in current)) {
      // Handle deleted field
      changes[key] = {
        label: this.getControlLabel(childControl, childPath),
        formControlName: childPath,
        path: childPath,
        old: initial[key],
        new: undefined,
        status: 'deleted',
        sectionName: this.getSectionName(path)
      };
    }
    else if (!(key in initial)) {
      // Handle added field
      changes[key] = {
        label: this.getControlLabel(childControl, childPath),
        formControlName: childPath,
        path: childPath,
        old: undefined,
        new: current[key],
        status: 'added',
        sectionName: this.getSectionName(path)
      };
    }
    else if (!this.areValuesEqual(initial[key], current[key])) {
      // Handle modified field
      changes[key] = {
        label: this.getControlLabel(childControl, childPath),
        formControlName: childPath,
        path: childPath,
        old: initial[key],
        new: current[key],
        status: 'modified',
        sectionName: this.getSectionName(path)
      };
    }
  });

  return changes;
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
