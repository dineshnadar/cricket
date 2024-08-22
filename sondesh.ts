import { Injectable, signal, computed, Signal, effect, untracked } from '@angular/core';
import { FormGroup, FormArray, AbstractControl, ValidatorFn, ValidationErrors, Validators } from '@angular/forms';

interface ExtendedControlOptions {
  label?: string;
  fldName?: string;
  visible?: boolean;
  editable?: boolean;
  lookup?: string;
  lookupData?: Array<{ label: string; value: any }>;
  regex?: string[];
  fldMax?: number;
  fieldType?: string;
  dfltVal?: any;
  required?: boolean;
  oldValue?: any;
  expand?: boolean;
  sectionName?: string;
  subSectionName?: string;
  reqBorder?: boolean;
  subName?: string;
  align?: string;
}

type ExtendedControlProperties = {
  [K in keyof ExtendedControlOptions]: NonNullable<ExtendedControlOptions[K]>;
} & {
  _oldValue?: any;
  _customComputation?: (control: AbstractControl, form: FormGroup) => any;
};

type ExtendedAbstractControl = AbstractControl & ExtendedControlProperties;

interface ReadViewItem {
  label: string;
  fldName: string;
  value: any;
  oldValue?: any;
  editable: boolean;
  visible: boolean;
  computedValue?: any;
  expand: boolean;
  sectionName?: string;
  subSectionName?: string;
  children?: ReadViewItem[];
  reqBorder?: boolean;
  subName?: string;
  align?: string;
  side?: string;
  seq?: number;
}

interface LayoutSection {
  type: 'leftHeader' | 'leftRightHeader' | 'commonHeader' | 'accordion' | 'accordionHeader';
  leftHeader?: string;
  rightHeader?: string;
  commonHeader?: string;
  accordionHeader?: string;
  fields: Array<{name: string; side: 'left' | 'right' | 'full'; seq: number}>;
}

interface FormLayout {
  type: 'standard' | 'noRightHeader' | 'multipleSection';
  sections: LayoutSection[];
}

@Injectable({
  providedIn: 'root'
})
export class FormExtensionService {
  private updateTrigger = signal(0);
  private readViewCache = new WeakMap<AbstractControl, Signal<ReadViewItem[]>>();
  private formLayouts = new WeakMap<FormGroup, FormLayout>();
  private extendedPropertiesCache = new WeakMap<AbstractControl, ExtendedControlProperties>();
  private customValidatorsCache = new WeakMap<AbstractControl, ValidatorFn[]>();

  constructor() {
    effect(() => {
      this.updateTrigger();
      this.clearCaches();
    });
  }

  extendControl(control: AbstractControl, options: ExtendedControlOptions): void {
    const extendedControl = control as ExtendedAbstractControl;
    let changed = false;

    Object.entries(options).forEach(([key, value]) => {
      if (key === 'regex') {
        if (!this.areRegexArraysEqual(extendedControl[key], value)) {
          extendedControl[key] = value;
          changed = true;
        }
      } else if (extendedControl[key] !== value) {
        extendedControl[key] = value;
        changed = true;
      }
    });

    if (changed) {
      if (control instanceof FormGroup || control instanceof FormArray) {
        this.extendNestedControls(control);
      }
      this.updateValidators(control);
      this.triggerUpdate();
    }
  }

  private extendNestedControls(control: FormGroup | FormArray): void {
    Object.values(control.controls).forEach(childControl => {
      if (!this.hasExtendedProperties(childControl)) {
        this.extendControl(childControl, {});
      }
    });
  }

  private hasExtendedProperties(control: AbstractControl): boolean {
    return 'label' in control || 'fldName' in control;
  }

  updateControlProperty<K extends keyof ExtendedControlOptions>(
    control: AbstractControl,
    property: K,
    value: ExtendedControlOptions[K]
  ): void {
    const extendedControl = control as ExtendedAbstractControl;
    if (property === 'regex') {
      if (!this.areRegexArraysEqual(extendedControl[property], value as string[])) {
        extendedControl[property] = value as any;
        this.updateValidators(control);
        this.triggerUpdate();
      }
    } else if (extendedControl[property] !== value) {
      extendedControl[property] = value as any;
      if (property === 'required') {
        this.updateValidators(control);
      }
      this.triggerUpdate();
    }
  }

  private areRegexArraysEqual(arr1?: string[], arr2?: string[]): boolean {
    if (!arr1 && !arr2) return true;
    if (!arr1 || !arr2) return false;
    if (arr1.length !== arr2.length) return false;
    return arr1.every((regex, index) => regex === arr2[index]);
  }

  setFormGroupLayout(formGroup: FormGroup, layout: FormLayout): void {
    const sortedLayout: FormLayout = {
      ...layout,
      sections: layout.sections.map(section => ({
        ...section,
        fields: [...section.fields].sort((a, b) => a.seq - b.seq)
      }))
    };
    this.formLayouts.set(formGroup, sortedLayout);
    this.triggerUpdate();
  }

  getExtendedReadView(control: AbstractControl): Signal<ReadViewItem[]> {
    if (!this.readViewCache.has(control)) {
      const readViewSignal = computed(() => {
        this.updateTrigger(); // Depend on the update trigger
        return untracked(() => this.getExtendedReadViewForControl(control));
      });
      this.readViewCache.set(control, readViewSignal);
    }
    return this.readViewCache.get(control)!;
  }

  private getExtendedReadViewForControl(control: AbstractControl): ReadViewItem[] {
    if (control instanceof FormGroup) {
      const layout = this.formLayouts.get(control);
      if (layout) {
        return this.applyLayoutToFormGroup(control, layout);
      }
    }
    return this.getBasicReadViewForControl(control);
  }

  private applyLayoutToFormGroup(formGroup: FormGroup, layout: FormLayout): ReadViewItem[] {
    return layout.sections.map(section => ({
      label: this.getSectionLabel(section),
      fldName: `section_${section.type}`,
      value: null,
      editable: false,
      visible: true,
      expand: true,
      children: section.fields
        .map(field => this.createReadViewItem(formGroup.get(field.name), field))
        .filter((item): item is ReadViewItem => item !== null),
    }));
  }

  private getBasicReadViewForControl(control: AbstractControl): ReadViewItem[] {
    const item = this.createReadViewItem(control);
    if (!item) return [];

    if (control instanceof FormGroup || control instanceof FormArray) {
      item.children = Object.entries(control.controls)
        .map(([key, childControl]) => this.createReadViewItem(childControl, { name: key, side: 'full', seq: 0 }))
        .filter((child): child is ReadViewItem => child !== null);
    }

    return [item];
  }

  private createReadViewItem(control: AbstractControl | null, field?: {name: string; side: string; seq: number}): ReadViewItem | null {
    if (!control) return null;
    const extendedControl = control as ExtendedAbstractControl;
    const rawValue = control.value;
    const transformedValue = this.transformValue(rawValue, extendedControl);

    const item: ReadViewItem = {
      label: extendedControl.label || field?.name || '',
      fldName: extendedControl.fldName || field?.name || '',
      value: transformedValue,
      oldValue: extendedControl._oldValue,
      editable: extendedControl.editable ?? true,
      visible: extendedControl.visible ?? true,
      expand: extendedControl.expand ?? false,
      sectionName: extendedControl.sectionName,
      subSectionName: extendedControl.subSectionName,
      reqBorder: extendedControl.reqBorder ?? false,
      subName: extendedControl.subName,
      align: extendedControl.align
    };

    if (field) {
      item.side = field.side;
      item.seq = field.seq;
    }

    if (extendedControl._customComputation) {
      item.computedValue = extendedControl._customComputation(control, control.root as FormGroup);
    }

    return item;
  }

  private transformValue(value: any, control: ExtendedAbstractControl): any {
    if (value === 'Y') return 'Yes';
    if (value === 'N') return 'No';

    if (control.lookupData && Array.isArray(control.lookupData)) {
      const matchingLookup = control.lookupData.find(item => item.value === value);
      if (matchingLookup) return matchingLookup.label;
    }

    return value;
  }

  private getSectionLabel(section: LayoutSection): string {
    switch (section.type) {
      case 'leftHeader':
      case 'leftRightHeader':
        return section.leftHeader || '';
      case 'commonHeader':
        return section.commonHeader || '';
      case 'accordion':
      case 'accordionHeader':
        return section.accordionHeader || '';
      default:
        return '';
    }
  }

  registerCustomComputation(control: AbstractControl, computation: (control: AbstractControl, form: FormGroup) => any): void {
    (control as ExtendedAbstractControl)._customComputation = computation;
    this.triggerUpdate();
  }

  applyDefaultValues(control: AbstractControl, storeOldValue: boolean = false): void {
    this.applyDefaultValuesRecursive(control, storeOldValue);
    this.triggerUpdate();
  }

  private applyDefaultValuesRecursive(control: AbstractControl, storeOldValue: boolean): void {
    const extendedControl = control as ExtendedAbstractControl;
    
    if (control instanceof FormGroup) {
      if (extendedControl.dfltVal !== undefined && this.isFormGroupEmpty(control)) {
        if (storeOldValue) {
          extendedControl._oldValue = this.getFormGroupValue(control);
        }
        this.setFormGroupValue(control, extendedControl.dfltVal);
      } else {
        Object.values(control.controls).forEach(childControl => {
          this.applyDefaultValuesRecursive(childControl, storeOldValue);
        });
      }
    } else if (control instanceof FormArray) {
      control.controls.forEach(ctrl => {
        this.applyDefaultValuesRecursive(ctrl, storeOldValue);
      });
    } else if (extendedControl.dfltVal !== undefined && control.value === null) {
      if (storeOldValue) {
        extendedControl._oldValue = control.value;
      }
      control.setValue(extendedControl.dfltVal);
    }
  }

  private isFormGroupEmpty(group: FormGroup): boolean {
    return Object.values(group.controls).every(control =>
      control.value === null || control.value === ''
    );
  }

  private getFormGroupValue(group: FormGroup): any {
    const value: any = {};
    Object.entries(group.controls).forEach(([key, control]) => {
      value[key] = control.value;
    });
    return value;
  }

  private setFormGroupValue(group: FormGroup, value: any): void {
    if (typeof value === 'object' && value !== null) {
      Object.entries(value).forEach(([key, val]) => {
        if (group.get(key)) {
          group.get(key)!.setValue(val);
        }
      });
    }
  }

  getOldValue(control: AbstractControl): any | undefined {
    return (control as ExtendedAbstractControl)._oldValue;
  }

  getExtendedProperties(control: AbstractControl): Partial<ExtendedControlProperties> {
    if (!this.extendedPropertiesCache.has(control)) {
      const extendedControl = control as ExtendedAbstractControl;
      const properties: Partial<ExtendedControlProperties> = {
        label: extendedControl.label,
        fldName: extendedControl.fldName,
        visible: extendedControl.visible,
        editable: extendedControl.editable,
        lookup: extendedControl.lookup,
        lookupData: extendedControl.lookupData,
        regex: extendedControl.regex,
        fldMax: extendedControl.fldMax,
        fieldType: extendedControl.fieldType,
        dfltVal: extendedControl.dfltVal,
        required: extendedControl.required,
        oldValue: extendedControl._oldValue,
        expand: extendedControl.expand,
        sectionName: extendedControl.sectionName,
        subSectionName: extendedControl.subSectionName,
        reqBorder: extendedControl.reqBorder,
        subName: extendedControl.subName,
        align: extendedControl.align
      };
      this.extendedPropertiesCache.set(control, properties);
    }
    return this.extendedPropertiesCache.get(control)!;
  }

  batchUpdates(updates: () => void): void {
    updates();
    this.triggerUpdate();
  }

  private triggerUpdate(): void {
    this.updateTrigger.update(v => v + 1);
  }

  private clearCaches(): void {
    this.readViewCache = new WeakMap();
    this.extendedPropertiesCache = new WeakMap();
  }

  private updateValidators(control: AbstractControl): void {
    const extendedControl = control as ExtendedAbstractControl;
    const validators: ValidatorFn[] = [];

    if (extendedControl.required) {
      validators.push(Validators.required);
    }

    if (extendedControl.regex && extendedControl.regex.length > 0) {
      validators.push(Validators.pattern(extendedControl.regex.join('|')));
    }

    if (extendedControl.fldMax !== undefined) {
      validators.push(Validators.max(extendedControl.fldMax));
    }

    // Add custom validators
    const customValidators = this.customValidatorsCache.get(control) || [];
    validators.push(...customValidators);

    control.setValidators(validators);
    control.updateValueAndValidity();
  }

addCustomValidator(control: AbstractControl, validator: ValidatorFn): void {
    const currentValidators = this.customValidatorsCache.get(control) || [];
    currentValidators.push(validator);
    this.customValidatorsCache.set(control, currentValidators);
    this.updateValidators(control);
  }

  removeCustomValidator(control: AbstractControl, validator: ValidatorFn): void {
    const currentValidators = this.customValidatorsCache.get(control) || [];
    const index = currentValidators.indexOf(validator);
    if (index > -1) {
      currentValidators.splice(index, 1);
      this.customValidatorsCache.set(control, currentValidators);
      this.updateValidators(control);
    }
  }

  // Utility method to get raw form group value
  getRawFormGroupValue(formGroup: FormGroup): any {
    const rawValue: any = {};
    Object.keys(formGroup.controls).forEach(key => {
      const control = formGroup.get(key);
      if (control instanceof FormGroup) {
        rawValue[key] = this.getRawFormGroupValue(control);
      } else if (control instanceof FormArray) {
        rawValue[key] = this.getRawFormArrayValue(control);
      } else {
        rawValue[key] = control?.value;
      }
    });
    return rawValue;
  }

  private getRawFormArrayValue(formArray: FormArray): any[] {
    return formArray.controls.map(control => {
      if (control instanceof FormGroup) {
        return this.getRawFormGroupValue(control);
      } else if (control instanceof FormArray) {
        return this.getRawFormArrayValue(control);
      } else {
        return control.value;
      }
    });
  }

  // Utility method to set raw form group value
  setRawFormGroupValue(formGroup: FormGroup, value: any): void {
    Object.keys(value).forEach(key => {
      const control = formGroup.get(key);
      if (control instanceof FormGroup) {
        this.setRawFormGroupValue(control, value[key]);
      } else if (control instanceof FormArray) {
        this.setRawFormArrayValue(control, value[key]);
      } else if (control) {
        control.setValue(value[key]);
      }
    });
  }

  private setRawFormArrayValue(formArray: FormArray, values: any[]): void {
    // Clear existing controls
    while (formArray.length !== 0) {
      formArray.removeAt(0);
    }
    
    // Add new controls
    values.forEach(value => {
      if (typeof value === 'object' && value !== null) {
        const group = new FormGroup({});
        this.setRawFormGroupValue(group, value);
        formArray.push(group);
      } else {
        formArray.push(new FormControl(value));
      }
    });
  }

  // Utility method to reset form to initial state
  resetForm(formGroup: FormGroup): void {
    formGroup.reset();
    this.applyDefaultValues(formGroup);
    this.triggerUpdate();
  }

  // Utility method to check if form is dirty
  isFormDirty(formGroup: FormGroup): boolean {
    return formGroup.dirty;
  }

  // Utility method to get all validation errors
  getAllErrors(formGroup: FormGroup): { [key: string]: any } {
    const errors: { [key: string]: any } = {};
    Object.keys(formGroup.controls).forEach(key => {
      const control = formGroup.get(key);
      if (control instanceof FormGroup) {
        const nestedErrors = this.getAllErrors(control);
        if (Object.keys(nestedErrors).length > 0) {
          errors[key] = nestedErrors;
        }
      } else {
        const controlErrors = control?.errors;
        if (controlErrors) {
          errors[key] = controlErrors;
        }
      }
    });
    return errors;
  }

  // Utility method to mark all controls as touched
  markAllAsTouched(formGroup: FormGroup): void {
    Object.values(formGroup.controls).forEach(control => {
      if (control instanceof FormGroup) {
        this.markAllAsTouched(control);
      } else {
        control.markAsTouched();
      }
    });
  }

  // Performance monitoring methods
  private performanceMonitor = new Map<string, number>();

  startPerformanceMonitor(methodName: string): void {
    this.performanceMonitor.set(methodName, performance.now());
  }

  endPerformanceMonitor(methodName: string): void {
    const startTime = this.performanceMonitor.get(methodName);
    if (startTime) {
      const endTime = performance.now();
      const duration = endTime - startTime;
      console.log(`Performance: ${methodName} took ${duration.toFixed(2)}ms`);
      this.performanceMonitor.delete(methodName);
    }
  }
}
