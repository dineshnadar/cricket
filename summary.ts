private getControlByPath(control: AbstractControl, path: string): AbstractControl | null {
  if (!path) return null;
  
  // Handle array paths like '[0]', '[1].partyname.username.firstName'
  const segments = path
    .replace(/\[(\d+)\]/g, '.$1') // Convert [0] to .0
    .split('.')
    .filter(Boolean);

  let currentControl: AbstractControl | null = control;

  for (const segment of segments) {
    if (!currentControl) return null;

    if (currentControl instanceof FormGroup) {
      currentControl = currentControl.get(segment);
    } 
    else if (currentControl instanceof FormArray) {
      const index = parseInt(segment);
      if (isNaN(index)) return null;
      currentControl = currentControl.at(index);
    } 
    else {
      return null;
    }
  }

  return currentControl;
}

// Updated updateControlChangeStatus to handle deep nesting
private updateControlChangeStatus(control: AbstractControl, changes: ChangeItem[]): void {
  // First reset all controls recursively
  this.resetControlChangeStatus(control);

  // Update status for changed controls
  changes.forEach(change => {
    // Get the control at the path
    const targetControl = this.getControlByPath(control, change.path);
    
    if (targetControl) {
      // Update the control and all its parents
      this.updateControlAndParentStatus(targetControl, change.status);
    }
  });
}

private updateControlAndParentStatus(control: AbstractControl, status: ChangeStatus): void {
  // Update current control
  this.synFormExtension.extendControl(control, {
    hasChanges: true,
    changeType: status
  } as ExtendedControlOptions);

  // If control is a FormGroup or FormArray, update all child controls
  if (control instanceof FormGroup || control instanceof FormArray) {
    this.updateChildrenStatus(control, status);
  }

  // Update all parent controls
  let parent = control.parent;
  while (parent) {
    this.synFormExtension.extendControl(parent, {
      hasChanges: true,
      changeType: 'modified'
    } as ExtendedControlOptions);
    parent = parent.parent;
  }
}

private updateChildrenStatus(control: AbstractControl, status: ChangeStatus): void {
  if (control instanceof FormGroup) {
    Object.values(control.controls).forEach(childControl => {
      this.synFormExtension.extendControl(childControl, {
        hasChanges: true,
        changeType: status
      } as ExtendedControlOptions);

      // Recursively update children
      if (childControl instanceof FormGroup || childControl instanceof FormArray) {
        this.updateChildrenStatus(childControl, status);
      }
    });
  } else if (control instanceof FormArray) {
    control.controls.forEach(childControl => {
      this.synFormExtension.extendControl(childControl, {
        hasChanges: true,
        changeType: status
      } as ExtendedControlOptions);

      // Recursively update children
      if (childControl instanceof FormGroup || childControl instanceof FormArray) {
        this.updateChildrenStatus(childControl, status);
      }
    });
  }
}
-------------
private updateControlChangeStatus(control: AbstractControl, changes: ChangeItem[]): void {
  // First reset all controls
  this.resetControlChangeStatus(control);

  // Update status for changed controls
  changes.forEach(change => {
    // Handle array paths like '[0]', '[1]', etc.
    const arrayPath = change.path.replace(/^\[(\d+)\]$/, '$1');
    let targetControl: AbstractControl | null = null;

    if (control instanceof FormArray) {
      // Direct array access using index
      targetControl = control.at(parseInt(arrayPath));
    } else {
      // Get nested control using path
      targetControl = control.get(arrayPath);
    }

    if (targetControl) {
      // Update the control status
      this.synFormExtension.extendControl(targetControl, {
        hasChanges: true,
        changeType: change.status
      } as ExtendedControlOptions);

      // Update parent controls
      this.updateParentControlStatus(targetControl);
    }
  });
}

// types.ts
export interface ChangeItem {
  label: string;
  formControlName: string;
  path: string;
  old: any;
  new: any;
  status: ChangeStatus;
  sectionName?: string;
}

export type ChangeStatus = 'added' | 'modified' | 'deleted' | 'unmodified';

export interface ChangeSummary {
  changes: ChangeItem[];
  hasChanges: boolean;
  addedCount: number;
  modifiedCount: number;
  deletedCount: number;
  sections: { [key: string]: ChangeItem[] };
}

export interface ExtendedControlOptions extends Record<string, any> {
  label?: string;
  formControlName?: string;
  _oldValue?: any;
  hasChanges?: boolean;
  changeType?: ChangeStatus;
  sectionName?: string;
}

export interface ExtendedAbstractControl extends AbstractControl {
  label?: string;
  formControlName?: string;
  _oldValue?: any;
  hasChanges?: boolean;
  changeType?: ChangeStatus;
  sectionName?: string;
}

// form-change.service.ts
@Injectable({
  providedIn: 'root'
})
export class FormChangeService {
  private readonly PRIMITIVE_TYPES = ['string', 'number', 'boolean'];
  
  constructor(
    private synFormExtension: SynFormExtensionService,
    private errorHandler: ErrorHandler
  ) {}

  /**
   * Initialize form with old values
   */
  initializeFormWithOldValues(form: FormGroup, oldValues: any): void {
    try {
      this.setOldValuesRecursively(form, oldValues);
    } catch (error) {
      this.handleError('Error initializing form values', error);
    }
  }

  /**
   * Get change summary for form
   */
  getChangeSummary(form: FormGroup): ChangeSummary {
    try {
      const currentValues = this.getFormValues(form);
      const oldValues = this.getOldValues(form);
      const changes = this.compareStates(oldValues, currentValues, '', form);
      const flattenedChanges = this.flattenChanges(changes);

      // Update control change status
      this.updateControlChangeStatus(form, flattenedChanges);

      return this.createChangeSummary(flattenedChanges);
    } catch (error) {
      this.handleError('Error getting change summary', error);
      return this.createEmptyChangeSummary();
    }
  }

  /**
   * Reset form to old values
   */
  resetForm(form: FormGroup): void {
    try {
      this.resetFormRecursively(form);
      this.resetControlChangeStatus(form);
    } catch (error) {
      this.handleError('Error resetting form', error);
    }
  }

  /**
   * Check if form has any changes
   */
  hasFormChanges(form: FormGroup): boolean {
    return (form as ExtendedAbstractControl).hasChanges || false;
  }

  /**
   * Check if specific control has changes
   */
  hasControlChanges(form: FormGroup, path: string): boolean {
    const control = this.getControlByPath(form, path);
    return control ? (control as ExtendedAbstractControl).hasChanges : false;
  }

  /**
   * Private helper methods
   */
  private setOldValuesRecursively(
    control: AbstractControl, 
    oldValue: any, 
    path: string = ''
  ): void {
    if (!control) return;

    if (control instanceof FormGroup) {
      this.handleFormGroupOldValues(control, oldValue, path);
    } 
    else if (control instanceof FormArray) {
      this.handleFormArrayOldValues(control, oldValue, path);
    } 
    else {
      this.handleFormControlOldValues(control, oldValue, path);
    }
  }

  private handleFormGroupOldValues(
    group: FormGroup, 
    oldValue: any, 
    path: string
  ): void {
    // Extend FormGroup
    this.extendControlWithValues(group, oldValue, path);

    // Process child controls
    Object.keys(group.controls).forEach(key => {
      const childPath = path ? `${path}.${key}` : key;
      const childControl = group.get(key);
      const childOldValue = oldValue?.[key];

      if (childControl) {
        this.setOldValuesRecursively(childControl, childOldValue, childPath);
      }
    });
  }

  private handleFormArrayOldValues(
    array: FormArray, 
    oldValue: any, 
    path: string
  ): void {
    // Extend FormArray
    this.extendControlWithValues(array, oldValue, path);

    // Process array items
    const arrayOldValue = Array.isArray(oldValue) ? oldValue : [];
    array.controls.forEach((control, index) => {
      const childPath = `${path}[${index}]`;
      this.setOldValuesRecursively(control, arrayOldValue[index], childPath);
    });
  }

  private handleFormControlOldValues(
    control: AbstractControl, 
    oldValue: any, 
    path: string
  ): void {
    this.extendControlWithValues(control, oldValue, path);
  }

  private extendControlWithValues(
    control: AbstractControl, 
    oldValue: any, 
    path: string
  ): void {
    const options: ExtendedControlOptions = {
      _oldValue: oldValue,
      hasChanges: false,
      changeType: 'unmodified',
      label: this.getControlLabel(control, path),
      formControlName: this.getFormControlName(path),
      sectionName: this.getSectionName(path)
    };

    this.synFormExtension.extendControl(control, options);
  }

  private compareStates(
    initial: any, 
    current: any, 
    path: string,
    control?: AbstractControl
  ): Partial<ChangeItem> {
    try {
      if (this.areValuesEqual(initial, current)) return {};

      if (Array.isArray(initial) && Array.isArray(current)) {
        return this.compareArrays(initial, current, path, control as FormArray);
      }

      if (this.isObject(initial) && this.isObject(current)) {
        return this.compareObjects(initial, current, path, control as FormGroup);
      }

      return this.createChangeItem({
        path,
        label: this.getControlLabel(control, path),
        status: this.determineChangeStatus(initial, current)
      }, initial, current);
    } catch (error) {
      this.handleError(`Error comparing states at path: ${path}`, error);
      return {};
    }
  }

  private compareArrays(
    initial: any[], 
    current: any[], 
    path: string,
    control?: FormArray
  ): Record<string, any> {
    const changes: Record<string, any> = {};
    let arrayStatus: ChangeStatus = 'unmodified';

    try {
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
        } 
        else if (i >= initial.length) {
          changes[i] = this.createArrayItemChange(
            itemPath, 
            itemControl, 
            undefined, 
            current[i], 
            'added'
          );
          arrayStatus = 'modified';
        } 
        else {
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
    } catch (error) {
      this.handleError(`Error comparing arrays at path: ${path}`, error);
      return changes;
    }
  }

  private compareObjects(
    initial: Record<string, any>, 
    current: Record<string, any>,
    path: string,
    control?: FormGroup
  ): Record<string, any> {
    const changes: Record<string, any> = {};
    let objectStatus: ChangeStatus = 'unmodified';

    try {
      const allKeys = new Set([...Object.keys(initial), ...Object.keys(current)]);

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
        } 
        else if (!(key in initial)) {
          changes[key] = this.createObjectPropertyChange(
            propertyPath, 
            propertyControl, 
            undefined, 
            current[key], 
            'added'
          );
          objectStatus = 'modified';
        } 
        else {
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
    } catch (error) {
      this.handleError(`Error comparing objects at path: ${path}`, error);
      return changes;
    }
  }

 // Continuing form-change.service.ts...

  private createChangeItem(
    metadata: { path: string; label: string; status: ChangeStatus },
    oldValue: any,
    newValue: any
  ): Partial<ChangeItem> {
    return {
      label: metadata.label,
      formControlName: this.getFormControlName(metadata.path),
      old: oldValue,
      new: newValue,
      status: metadata.status,
      path: metadata.path,
      sectionName: this.getSectionName(metadata.path)
    };
  }

  private createArrayItemChange(
    path: string,
    control: AbstractControl | null,
    oldValue: any,
    newValue: any,
    status: ChangeStatus
  ): Partial<ChangeItem> {
    return this.createChangeItem(
      {
        path,
        label: this.getControlLabel(control, path),
        status
      },
      oldValue,
      newValue
    );
  }

  private createObjectPropertyChange(
    path: string,
    control: AbstractControl | null,
    oldValue: any,
    newValue: any,
    status: ChangeStatus
  ): Partial<ChangeItem> {
    return this.createChangeItem(
      {
        path,
        label: this.getControlLabel(control, path),
        status
      },
      oldValue,
      newValue
    );
  }

  private flattenChanges(changes: Record<string, any>): ChangeItem[] {
    const flattened: ChangeItem[] = [];

    const processChanges = (obj: Record<string, any>, parentPath: string = '') => {
      for (const [key, value] of Object.entries(obj)) {
        if (this.isChangeItem(value)) {
          flattened.push(value as ChangeItem);
        } else if (this.isObject(value) && key !== 'status') {
          processChanges(
            value,
            parentPath ? `${parentPath}.${key}` : key
          );
        }
      }
    };

    processChanges(changes);
    return flattened;
  }

  private createChangeSummary(changes: ChangeItem[]): ChangeSummary {
    const sections: { [key: string]: ChangeItem[] } = {};

    // Group changes by section
    changes.forEach(change => {
      const sectionName = change.sectionName || 'other';
      if (!sections[sectionName]) {
        sections[sectionName] = [];
      }
      sections[sectionName].push(change);
    });

    return {
      changes,
      hasChanges: changes.length > 0,
      addedCount: changes.filter(c => c.status === 'added').length,
      modifiedCount: changes.filter(c => c.status === 'modified').length,
      deletedCount: changes.filter(c => c.status === 'deleted').length,
      sections
    };
  }

  private createEmptyChangeSummary(): ChangeSummary {
    return {
      changes: [],
      hasChanges: false,
      addedCount: 0,
      modifiedCount: 0,
      deletedCount: 0,
      sections: {}
    };
  }

  private updateControlChangeStatus(form: FormGroup, changes: ChangeItem[]): void {
    // Reset all controls first
    this.resetControlChangeStatus(form);

    // Update status for changed controls
    changes.forEach(change => {
      const control = this.getControlByPath(form, change.path);
      if (control) {
        this.synFormExtension.extendControl(control, {
          hasChanges: true,
          changeType: change.status
        } as ExtendedControlOptions);

        // Update parent controls
        this.updateParentControlStatus(control);
      }
    });
  }

  private resetControlChangeStatus(control: AbstractControl): void {
    this.synFormExtension.extendControl(control, {
      hasChanges: false,
      changeType: 'unmodified'
    } as ExtendedControlOptions);

    if (control instanceof FormGroup) {
      Object.values(control.controls).forEach(child => {
        this.resetControlChangeStatus(child);
      });
    } else if (control instanceof FormArray) {
      control.controls.forEach(child => {
        this.resetControlChangeStatus(child);
      });
    }
  }

  private updateParentControlStatus(control: AbstractControl): void {
    let parent = control.parent;
    while (parent) {
      this.synFormExtension.extendControl(parent, {
        hasChanges: true,
        changeType: 'modified'
      } as ExtendedControlOptions);
      parent = parent.parent;
    }
  }

  private getFormValues(control: AbstractControl): any {
    if (control instanceof FormGroup) {
      return this.getFormGroupValues(control);
    } else if (control instanceof FormArray) {
      return this.getFormArrayValues(control);
    }
    return control.value;
  }

  private getFormGroupValues(group: FormGroup): Record<string, any> {
    return Object.keys(group.controls).reduce((values, key) => {
      values[key] = this.getFormValues(group.get(key)!);
      return values;
    }, {} as Record<string, any>);
  }

  private getFormArrayValues(array: FormArray): any[] {
    return array.controls.map(control => this.getFormValues(control));
  }

  private getOldValues(control: AbstractControl): any {
    if (control instanceof FormGroup) {
      return this.getOldFormGroupValues(control);
    } else if (control instanceof FormArray) {
      return this.getOldFormArrayValues(control);
    }
    return (control as ExtendedAbstractControl)._oldValue;
  }

  private getOldFormGroupValues(group: FormGroup): Record<string, any> {
    return Object.keys(group.controls).reduce((values, key) => {
      values[key] = this.getOldValues(group.get(key)!);
      return values;
    }, {} as Record<string, any>);
  }

  private getOldFormArrayValues(array: FormArray): any[] {
    return array.controls.map(control => this.getOldValues(control));
  }

  private resetFormRecursively(control: AbstractControl): void {
    const extendedControl = control as ExtendedAbstractControl;
    
    if (control instanceof FormGroup) {
      Object.values(control.controls).forEach(child => {
        this.resetFormRecursively(child);
      });
    } else if (control instanceof FormArray) {
      control.controls.forEach(child => {
        this.resetFormRecursively(child);
      });
    } else if (extendedControl._oldValue !== undefined) {
      control.setValue(extendedControl._oldValue);
    }
  }

  private getControlByPath(form: FormGroup, path: string): AbstractControl | null {
    if (!path) return null;
    
    const parts = path.split(/[.\[\]]+/).filter(Boolean);
    let current: AbstractControl = form;

    for (const part of parts) {
      if (current instanceof FormGroup) {
        current = current.get(part)!;
      } else if (current instanceof FormArray) {
        current = current.at(parseInt(part));
      }

      if (!current) return null;
    }

    return current;
  }

  private getControlLabel(control?: AbstractControl, fallbackPath?: string): string {
    if (control) {
      const extendedControl = control as ExtendedAbstractControl;
      if (extendedControl.label) {
        return extendedControl.label;
      }
    }
    return this.generateLabelFromPath(fallbackPath || '');
  }

  private generateLabelFromPath(path: string): string {
    const parts = path.split(/[.\[\]]+/).filter(Boolean);
    return parts[parts.length - 1] || path;
  }

  private getFormControlName(path: string): string {
    return path.replace(/\[\d+\]/g, '');
  }

  private getSectionName(path: string): string {
    return path.split('.')[0];
  }

  private determineChangeStatus(oldValue: any, newValue: any): ChangeStatus {
    if (oldValue === undefined || oldValue === null) return 'added';
    if (newValue === undefined || newValue === null) return 'deleted';
    return 'modified';
  }

  private areValuesEqual(a: any, b: any): boolean {
    if (a === b) return true;
    if (a === null || b === null) return false;
    if (typeof a !== typeof b) return false;
    
    if (this.PRIMITIVE_TYPES.includes(typeof a)) {
      return a === b;
    }
    
    return JSON.stringify(a) === JSON.stringify(b);
  }

  private isObject(value: any): boolean {
    return typeof value === 'object' && value !== null && !Array.isArray(value);
  }

  private isChangeItem(value: any): boolean {
    return this.isObject(value) && 
           'old' in value && 
           'new' in value && 
           'status' in value;
  }

  private handleError(message: string, error: any): void {
    console.error(message, error);
    this.errorHandler.handleError(error);
  }
}
