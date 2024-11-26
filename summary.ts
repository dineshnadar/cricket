// form-extension.service.ts
import { Injectable } from '@angular/core';
import { AbstractControl, FormArray, FormGroup, FormControl } from '@angular/forms';
import { SynFormExtensionService } from './syn-form-extension.service';
import { ExtendedControlOptions, ExtendedAbstractControl } from './syn-form-extension.type';

interface ChangeItem {
  label: string;
  old?: any;
  new?: any;
  path?: string;
  sectionName?: string;
}

@Injectable({
  providedIn: 'root'
})
export class FormExtensionService {
  private formLabels: { [key: string]: string } = {};
  private oldValueMap = new Map<string, any>();

  constructor(private synFormExtension: SynFormExtensionService) {}

  /**
   * Set old values from JSON and extend form controls
   */
  initializeFormWithOldValues(form: FormGroup, oldJson: any): void {
    this.setOldValuesRecursively(form, oldJson);
  }

  /**
   * Get changes between current form state and old values
   */
  getChangeSummary(form: FormGroup): ChangeItem[] {
    const currentValues = this.getFormValues(form);
    const oldValues = this.getOldValues(form);
    const changes = this.compareStates(oldValues, currentValues);
    return this.flattenChanges(changes);
  }

  /**
   * Recursively set old values and extend form controls
   */
  private setOldValuesRecursively(control: AbstractControl, oldValue: any, path: string = ''): void {
    if (control instanceof FormGroup) {
      Object.keys(control.controls).forEach(key => {
        const childPath = path ? `${path}.${key}` : key;
        const childControl = control.get(key);
        const childOldValue = oldValue?.[key];

        if (childControl) {
          this.setOldValuesRecursively(childControl, childOldValue, childPath);
        }
      });
    } 
    else if (control instanceof FormArray) {
      const arrayOldValue = Array.isArray(oldValue) ? oldValue : [];
      control.controls.forEach((childControl, index) => {
        const childPath = `${path}[${index}]`;
        this.setOldValuesRecursively(childControl, arrayOldValue[index], childPath);
      });
    } 
    else {
      // Store old value in map
      this.oldValueMap.set(path, oldValue);

      // Extend control with old value property
      this.synFormExtension.extendControl(control, {
        _oldValue: oldValue,
        fldName: path,
        label: this.getControlLabel(path)
      } as ExtendedControlOptions);

      // Store label if available
      const extendedControl = control as ExtendedAbstractControl;
      if (extendedControl.label) {
        this.formLabels[path] = extendedControl.label;
      }
    }
  }

  /**
   * Get current form values
   */
  private getFormValues(control: AbstractControl): any {
    if (control instanceof FormGroup) {
      const values: any = {};
      Object.keys(control.controls).forEach(key => {
        const childControl = control.get(key);
        if (childControl) {
          values[key] = this.getFormValues(childControl);
        }
      });
      return values;
    } 
    else if (control instanceof FormArray) {
      return control.controls.map(c => this.getFormValues(c));
    } 
    else {
      return control.value;
    }
  }

  /**
   * Get stored old values
   */
  private getOldValues(control: AbstractControl, path: string = ''): any {
    if (control instanceof FormGroup) {
      const values: any = {};
      Object.keys(control.controls).forEach(key => {
        const childPath = path ? `${path}.${key}` : key;
        const childControl = control.get(key);
        if (childControl) {
          values[key] = this.getOldValues(childControl, childPath);
        }
      });
      return values;
    } 
    else if (control instanceof FormArray) {
      return control.controls.map((c, i) => 
        this.getOldValues(c, `${path}[${i}]`)
      );
    } 
    else {
      return this.oldValueMap.get(path);
    }
  }

  /**
   * Compare states and generate change summary
   */
  private compareStates(initial: any, current: any, path: string = ''): any {
    // Handle null/undefined cases
    if (initial === current) return {};
    if (!initial && current) return { 
      label: this.getLabel(path), 
      old: initial, 
      new: current,
      path 
    };
    if (initial && !current) return { 
      label: this.getLabel(path), 
      old: initial, 
      new: current,
      path 
    };

    // Handle arrays
    if (Array.isArray(initial) && Array.isArray(current)) {
      const changes: any = {};
      const maxLength = Math.max(initial.length, current.length);
      
      for (let i = 0; i < maxLength; i++) {
        if (i >= current.length) {
          changes[i] = { 
            label: this.getLabel(`${path}[${i}]`), 
            old: initial[i], 
            new: undefined,
            path: `${path}[${i}]` 
          };
        } 
        else if (i >= initial.length) {
          changes[i] = { 
            label: this.getLabel(`${path}[${i}]`), 
            old: undefined, 
            new: current[i],
            path: `${path}[${i}]` 
          };
        } 
        else {
          const itemChanges = this.compareStates(
            initial[i], 
            current[i], 
            `${path}[${i}]`
          );
          if (Object.keys(itemChanges).length > 0) {
            changes[i] = itemChanges;
          }
        }
      }

      if (initial.length !== current.length) {
        changes.arrayChanges = { 
          label: 'Array Length', 
          old: initial.length, 
          new: current.length,
          path 
        };
      }

      return changes;
    }

    // Handle objects
    if (typeof initial === 'object' && initial !== null && 
        typeof current === 'object' && current !== null) {
      const changes: any = {};
      const allKeys = new Set([...Object.keys(initial), ...Object.keys(current)]);

      for (const key of allKeys) {
        const newPath = path ? `${path}.${key}` : key;
        const itemChanges = this.compareStates(
          initial[key], 
          current[key], 
          newPath
        );
        
        if (Object.keys(itemChanges).length > 0) {
          changes[key] = {
            label: this.getLabel(newPath),
            ...itemChanges,
            path: newPath
          };
        }
      }

      return changes;
    }

    // Handle primitive values
    if (initial !== current) {
      return { 
        label: this.getLabel(path), 
        old: initial, 
        new: current,
        path 
      };
    }

    return {};
  }

  /**
   * Flatten nested changes into array
   */
  private flattenChanges(changes: any): ChangeItem[] {
    const flattened: ChangeItem[] = [];

    const processChanges = (obj: any) => {
      for (const [key, value] of Object.entries(obj)) {
        if (value && typeof value === 'object') {
          if ('old' in value || 'new' in value) {
            // Extract section name from path if available
            const sectionName = this.extractSectionName(value.path);
            flattened.push({
              ...value,
              sectionName
            });
          } else {
            processChanges(value);
          }
        }
      }
    };

    processChanges(changes);
    return flattened;
  }

  /**
   * Get label for path
   */
  private getLabel(path: string): string {
    return this.formLabels[path] || this.getControlLabel(path);
  }

  /**
   * Generate control label from path
   */
  private getControlLabel(path: string): string {
    const parts = path.split(/[.\[\]]+/).filter(Boolean);
    return parts[parts.length - 1] || path;
  }

  /**
   * Extract section name from path
   */
  private extractSectionName(path: string = ''): string {
    const parts = path.split('.');
    return parts[0] || '';
  }

  /**
   * Clear stored data
   */
  clear(): void {
    this.formLabels = {};
    this.oldValueMap.clear();
  }
}

// Example usage:
@Component({
  template: `
    <div>
      <app-form-actions
        [isEditMode]="isEditMode()"
        [isValid]="form.valid"
        (continueClicked)="handleContinue()"
      ></app-form-actions>

      <app-common-form
        [formGroup]="form"
        [readViewData]="readViewData()"
        [isEditMode]="isEditMode()"
      ></app-common-form>

      <!-- Changes Dialog -->
      <app-changes-dialog
        *ngIf="showChanges()"
        [changes]="formChanges()"
        (confirmed)="submitForm()"
        (cancelled)="cancelChanges()"
      ></app-changes-dialog>
    </div>
  `
})
export class ParentComponent implements OnInit {
  form: FormGroup;
  isEditMode = signal(false);
  showChanges = signal(false);
  formChanges = signal<ChangeItem[]>([]);

  constructor(
    private fb: FormBuilder,
    private formExtension: FormExtensionService
  ) {
    this.form = this.fb.group({
      // ... form controls
    });
  }

  ngOnInit() {
    // Example old JSON data
    const oldData = {
      name: 'John Doe',
      address: {
        street: '123 Main St',
        city: 'Example City'
      },
      phones: [
        { type: 'home', number: '555-1234' },
        { type: 'work', number: '555-5678' }
      ]
    };

    // Initialize form with old values
    this.formExtension.initializeFormWithOldValues(this.form, oldData);
  }

  handleContinue() {
    if (this.form.valid) {
      const changes = this.formExtension.getChangeSummary(this.form);
      if (changes.length > 0) {
        this.formChanges.set(changes);
        this.showChanges.set(true);
      } else {
        this.submitForm();
      }
    }
  }

  submitForm() {
    // Handle form submission
    console.log('Form submitted:', this.form.value);
    this.showChanges.set(false);
    this.isEditMode.set(false);
  }

  cancelChanges() {
    this.showChanges.set(false);
  }
}
