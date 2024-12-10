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
    current?.forEach((value, index) => {
      const itemPath = `${path}[${index}]`;
      const itemControl = control?.at(index);

      if (itemControl instanceof FormGroup) {
        // For nested objects in array
        Object.keys(itemControl.controls).forEach(key => {
          const childPath = `${itemPath}.${key}`;
          const childControl = itemControl.get(key);
          const childValue = value[key];

          changes[index] = {
            label: this.getControlLabel(childControl, childPath),
            old: null,
            new: childValue,
            status: 'added'
          };
        });
      } else {
        // For simple values in array
        changes[index] = this.createArrayItemChange(
          itemPath,
          itemControl,
          null,
          value,
          'added'
        );
      }
      arrayStatus = 'modified';
    });

    if (Object.keys(changes).length > 0) {
      changes.status = arrayStatus;
    }

    return changes;
  }

  // Normal array comparison for existing values
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
