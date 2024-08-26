

--------------------------------------------------------

import { Injectable, signal, computed, Signal, effect, untracked } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, AbstractControl, ValidatorFn, ValidationErrors, Validators, FormControl } from '@angular/forms';

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
  isArray?: boolean;
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
  type: string;
  editable: boolean;
  visible: boolean;
  expand: boolean;
  children?: ReadViewItem[];
}

interface FieldItem extends ReadViewItem {
  value: any;
  oldValue?: any;
  side: string;
  seq: number;
  fieldType?: string;
  isArray?: boolean;
  lookupData?: Array<{ label: string; value: any }>;
  isValid?: boolean;
  errors?: ValidationErrors | null;
  status?: string;
  touched?: boolean;
  dirty?: boolean;
  required?: boolean;
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

interface UIReadViewOptions {
  includeSections?: string[];
  excludeSections?: string[];
  includeFields?: string[];
  excludeFields?: string[];
  includeOldValues?: boolean;
  includeErrors?: boolean;
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

  constructor(private fb: FormBuilder) {
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
    if (control instanceof FormGroup) {
      Object.values(control.controls).forEach(childControl => {
        if (!this.hasExtendedProperties(childControl)) {
          this.extendControl(childControl, {});
        }
      });
    } else if (control instanceof FormArray) {
      control.controls.forEach(childControl => {
        if (!this.hasExtendedProperties(childControl)) {
          this.extendControl(childControl, {});
        }
      });
    }
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
      type: section.type,
      editable: false,
      visible: true,
      expand: true,
      children: section.fields
        .map(field => this.createFieldItem(formGroup.get(field.name), field))
        .filter((item): item is FieldItem => item !== null)
    }));
  }

  private getBasicReadViewForControl(control: AbstractControl): ReadViewItem[] {
    const item = this.createFieldItem(control, { name: control instanceof FormGroup ? 'group' : 'control', side: 'full', seq: 0 });
    if (!item) return [];

    if (control instanceof FormGroup || control instanceof FormArray) {
      item.children = this.getChildrenReadViewItems(control);
    }

    return [item];
  }

  private createFieldItem(control: AbstractControl | null, field: {name: string; side: string; seq: number}): FieldItem | null {
    if (!control) return null;
    const extendedControl = control as ExtendedAbstractControl;
    
    return {
      label: extendedControl.label || field.name,
      fldName: field.name,
      type: 'field',
      value: control.value,
      oldValue: extendedControl._oldValue,
      editable: extendedControl.editable ?? true,
      visible: extendedControl.visible ?? true,
      side: field.side,
      seq: field.seq,
      fieldType: extendedControl.fieldType,
      isArray: extendedControl.isArray,
      expand: extendedControl.expand ?? false,
      lookupData: extendedControl.lookupData,
      isValid: control.valid,
      errors: control.errors,
      status: control.status,
      touched: control.touched,
      dirty: control.dirty,
      required: extendedControl.required
    };
  }

  private getChildrenReadViewItems(control: FormGroup | FormArray): FieldItem[] {
    if (control instanceof FormGroup) {
      return Object.entries(control.controls).map(([key, childControl]) => 
        this.createFieldItem(childControl, { name: key, side: 'full', seq: 0 })
      ).filter((item): item is FieldItem => item !== null);
    } else if (control instanceof FormArray) {
      return control.controls.map((childControl, index) => 
        this.createFieldItem(childControl, { name: index.toString(), side: 'full', seq: index })
      ).filter((item): item is FieldItem => item !== null);
    }
    return [];
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
        align: extendedControl.align,
        isArray: extendedControl.isArray
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

  handleArrayOperation(formArray: FormArray, operation: 'add' | 'remove', index?: number): void {
    const arrayExtendedControl = formArray as ExtendedAbstractControl;
    
    if (operation === 'add') {
      if (arrayExtendedControl.fieldType === 'arrayGroup') {
        const newGroup = this.createNewArrayGroupItem(formArray);
        formArray.push(newGroup);
      } else if (arrayExtendedControl.fieldType === 'arraySimple') {
        formArray.push(this.fb.control(null));
      }
    } else if (operation === 'remove' && index !== undefined) {
      formArray.removeAt(index);
    }

    this.triggerUpdate();
  }

  private createNewArrayGroupItem(formArray: FormArray): FormGroup {
    const template = (formArray.at(0) as FormGroup).controls;
    const newGroup = this.fb.group({});
    Object.entries(template).forEach(([key, control]) => {
      newGroup.addControl(key, this.fb.control(null, control.validator));
      this.extendControl(newGroup.get(key)!, this.getExtendedProperties(control));
    });
    return newGroup;
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

  // Method to get UI read view
  getUIReadView(formGroup: FormGroup, options: UIReadViewOptions = {}): ReadViewItem[] {
    const layout = this.formLayouts.get(formGroup);
    if (!layout) {
      console.warn('No layout found for this form group. Returning basic read view.');
      return this.getBasicReadViewForControl(formGroup);
    }

    return this.applyLayoutToFormGroup(formGroup, layout)
      .filter(section => this.shouldIncludeSection(section, options))
      .map(section => ({
        ...section,
        children: section.children
          ?.filter(field => this.shouldIncludeField(field as FieldItem, options))
          .map(field => this.applyFieldOptions(field as FieldItem, options)) || []
      }));
  }

  private shouldIncludeSection(section: ReadViewItem, options: UIReadViewOptions): boolean {
    if (options.includeSections && !options.includeSections.includes(section.type)) {
      return false;
    }
    if (options.excludeSections && options.excludeSections.includes(section.type)) {
      return false;
    }
    return true;
  }

  private shouldIncludeField(field: FieldItem, options: UIReadViewOptions): boolean {
    if (options.includeFields && !options.includeFields.includes(field.fldName)) {
      return false;
    }
    if (options.excludeFields && options.excludeFields.includes(field.fldName)) {
      return false;
    }
    return true;
  }

  private applyFieldOptions(field: FieldItem, options: UIReadViewOptions): FieldItem {
    const result = { ...field };
    if (!options.includeOldValues) {
      delete result.oldValue;
    }
    if (!options.includeErrors) {
      delete result.errors;
    }
    return result;
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
