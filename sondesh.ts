import { Injectable, signal, computed, Signal, effect, untracked } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, AbstractControl, ValidatorFn, ValidationErrors, Validators } from '@angular/forms';

interface UIReadViewOptions {
  includeSections?: string[];
  excludeSections?: string[];
  includeFields?: string[];
  excludeFields?: string[];
  includeOldValues?: boolean;
  includeErrors?: boolean;
}

interface UIReadViewItem extends ReadViewItem {
  side: string;
  seq: number;
  isValid: boolean;
  status: string;
  touched: boolean;
  dirty: boolean;
  oldValue?: any;
  errors?: ValidationErrors | null;
}

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
  fieldType?: string;
  isArray?: boolean;
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
      item.children = this.getChildrenReadViewItems(control);
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
      align: extendedControl.align,
      fieldType: extendedControl.fieldType,
      isArray: extendedControl.isArray
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

  private getChildrenReadViewItems(control: FormGroup | FormArray): ReadViewItem[] {
    if (control instanceof FormGroup) {
      return Object.entries(control.controls).map(([key, childControl]) => 
        this.createReadViewItem(childControl, { name: key, side: 'full', seq: 0 })
      ).filter((item): item is ReadViewItem => item !== null);
    } else if (control instanceof FormArray) {
      return control.controls.map((childControl, index) => 
        this.createReadViewItem(childControl, { name: index.toString(), side: 'full', seq: index })
      ).filter((item): item is ReadViewItem => item !== null);
    }
    return [];
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

  // Method to get only the read view
  getReadView(control: AbstractControl): ReadViewItem[] {
    return this.getBasicReadViewForControl(control);
  }

    getUIReadView(formGroup: FormGroup, options: UIReadViewOptions = {}): UIReadViewItem[] {
    const layout = this.formLayouts.get(formGroup);
    if (!layout) {
      console.warn('No layout found for this form group. Returning basic read view.');
      return this.getReadView(formGroup);
    }

    return this.applyLayoutToFormGroup(formGroup, layout, options);
  }

  private applyLayoutToFormGroup(formGroup: FormGroup, layout: FormLayout, options: UIReadViewOptions): UIReadViewItem[] {
    return layout.sections
      .filter(section => this.shouldIncludeSection(section, options))
      .map(section => ({
        type: section.type,
        label: this.getSectionLabel(section),
        fldName: `section_${section.type}`,
        editable: false,
        visible: true,
        expand: true,
        fields: section.fields
          .filter(field => this.shouldIncludeField(field, options))
          .map(field => this.createUIReadViewItem(formGroup.get(field.name), field, options))
          .filter((item): item is UIReadViewItem => item !== null)
      }));
  }

  private createUIReadViewItem(control: AbstractControl | null, field: {name: string; side: string; seq: number}, options: UIReadViewOptions): UIReadViewItem | null {
    if (!control) return null;

    const extendedControl = control as ExtendedAbstractControl;
    const basicItem = this.createReadViewItem(control, field);
    if (!basicItem) return null;

    const item: UIReadViewItem = {
      ...basicItem,
      side: field.side,
      seq: field.seq,
      isValid: control.valid,
      status: control.status,
      touched: control.touched,
      dirty: control.dirty
    };

    if (options.includeOldValues) {
      item.oldValue = extendedControl._oldValue;
    }

    if (options.includeErrors) {
      item.errors = control.errors;
    }

    return item;
  }

  private shouldIncludeSection(section: LayoutSection, options: UIReadViewOptions): boolean {
    if (options.includeSections && !options.includeSections.includes(section.type)) {
      return false;
    }
    if (options.excludeSections && options.excludeSections.includes(section.type)) {
      return false;
    }
    return true;
  }

  private shouldIncludeField(field: {name: string; side: string; seq: number}, options: UIReadViewOptions): boolean {
    if (options.includeFields && !options.includeFields.includes(field.name)) {
      return false;
    }
    if (options.excludeFields && options.excludeFields.includes(field.name)) {
      return false;
    }
    return true;
  }


----------

  // Get the full UI read view
const fullView = this.formExtensionService.getUIReadView(this.complexForm);

// Get only specific sections
const personalInfoView = this.formExtensionService.getUIReadView(this.complexForm, {
  includeSections: ['commonHeader']
});

// Exclude certain fields
const viewWithoutEmail = this.formExtensionService.getUIReadView(this.complexForm, {
  excludeFields: ['email']
});

// Include old values and errors
const detailedView = this.formExtensionService.getUIReadView(this.complexForm, {
  includeOldValues: true,
  includeErrors: true
});

// Combine multiple options
const customView = this.formExtensionService.getUIReadView(this.complexForm, {
  includeSections: ['commonHeader', 'accordion'],
  excludeFields: ['sensitive_data'],
  includeOldValues: true,
  includeErrors: false
});

-----------

  import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { FormExtensionService } from './form-extension.service';

@Component({
  selector: 'app-complex-form',
  template: `
    <form [formGroup]="complexForm" (ngSubmit)="onSubmit()">
      <!-- Form fields would be rendered here based on the UI read view -->
    </form>
    <button (click)="addSkill()">Add Skill</button>
    <button (click)="resetForm()">Reset Form</button>
    <h2>Form Data:</h2>
    <pre>{{ formData | json }}</pre>
    <h2>UI Read View:</h2>
    <pre>{{ uiReadView | json }}</pre>
  `
})
export class ComplexFormComponent implements OnInit {
  complexForm: FormGroup;
  formData: any;
  uiReadView: any;

  constructor(
    private fb: FormBuilder,
    private formExtensionService: FormExtensionService
  ) {}

  ngOnInit() {
    this.initForm();
    this.extendFormControls();
    this.setFormLayout();
    this.formExtensionService.applyDefaultValues(this.complexForm, true);
    this.updateViews();
  }

  private initForm() {
    this.complexForm = this.fb.group({
      personalInfo: this.fb.group({
        firstName: ['', Validators.required],
        lastName: ['', Validators.required],
        email: ['', [Validators.required, Validators.email]]
      }),
      address: this.fb.group({
        street: [''],
        city: [''],
        country: ['']
      }),
      skills: this.fb.array([]),
      preferences: this.fb.group({
        theme: ['light'],
        notifications: [true]
      })
    });
  }

  private extendFormControls() {
    // Extend personal info controls
    this.formExtensionService.extendControl(this.complexForm.get('personalInfo.firstName'), {
      label: 'First Name',
      fieldType: 'text',
      required: true
    });
    this.formExtensionService.extendControl(this.complexForm.get('personalInfo.lastName'), {
      label: 'Last Name',
      fieldType: 'text',
      required: true
    });
    this.formExtensionService.extendControl(this.complexForm.get('personalInfo.email'), {
      label: 'Email',
      fieldType: 'email',
      required: true
    });

    // Extend address controls
    this.formExtensionService.extendControl(this.complexForm.get('address.street'), {
      label: 'Street',
      fieldType: 'text'
    });
    this.formExtensionService.extendControl(this.complexForm.get('address.city'), {
      label: 'City',
      fieldType: 'text'
    });
    this.formExtensionService.extendControl(this.complexForm.get('address.country'), {
      label: 'Country',
      fieldType: 'text'
    });

    // Extend skills array
    this.formExtensionService.extendControl(this.complexForm.get('skills'), {
      label: 'Skills',
      fieldType: 'arraySimple',
      isArray: true
    });

    // Extend preferences controls
    this.formExtensionService.extendControl(this.complexForm.get('preferences.theme'), {
      label: 'Theme',
      fieldType: 'select',
      lookupData: [
        { label: 'Light', value: 'light' },
        { label: 'Dark', value: 'dark' }
      ]
    });
    this.formExtensionService.extendControl(this.complexForm.get('preferences.notifications'), {
      label: 'Enable Notifications',
      fieldType: 'checkbox'
    });
  }

  private setFormLayout() {
    const layout: FormLayout = {
      type: 'multipleSection',
      sections: [
        {
          type: 'commonHeader',
          commonHeader: 'Personal Information',
          fields: [
            { name: 'personalInfo.firstName', side: 'left', seq: 1 },
            { name: 'personalInfo.lastName', side: 'right', seq: 2 },
            { name: 'personalInfo.email', side: 'full', seq: 3 }
          ]
        },
        {
          type: 'commonHeader',
          commonHeader: 'Address',
          fields: [
            { name: 'address.street', side: 'full', seq: 1 },
            { name: 'address.city', side: 'left', seq: 2 },
            { name: 'address.country', side: 'right', seq: 3 }
          ]
        },
        {
          type: 'accordion',
          accordionHeader: 'Skills',
          fields: [
            { name: 'skills', side: 'full', seq: 1 }
          ]
        },
        {
          type: 'commonHeader',
          commonHeader: 'Preferences',
          fields: [
            { name: 'preferences.theme', side: 'left', seq: 1 },
            { name: 'preferences.notifications', side: 'right', seq: 2 }
          ]
        }
      ]
    };
    this.formExtensionService.setFormGroupLayout(this.complexForm, layout);
  }

  addSkill() {
    const skills = this.complexForm.get('skills') as FormArray;
    this.formExtensionService.handleArrayOperation(skills, 'add');
    this.updateViews();
  }

  resetForm() {
    this.formExtensionService.resetForm(this.complexForm);
    this.updateViews();
  }

  onSubmit() {
    if (this.complexForm.valid) {
      console.log('Form submitted:', this.formExtensionService.getRawFormGroupValue(this.complexForm));
    } else {
      console.log('Form is invalid');
      this.formExtensionService.markAllAsTouched(this.complexForm);
      console.log('Form errors:', this.formExtensionService.getAllErrors(this.complexForm));
    }
    this.updateViews();
  }

  private updateViews() {
    this.formData = this.formExtensionService.getRawFormGroupValue(this.complexForm);
    this.uiReadView = this.formExtensionService.getUIReadView(this.complexForm, {
      includeOldValues: true,
      includeErrors: true
    });
  }
}

------------

  import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { FormExtensionService } from './form-extension.service';

@Component({
  selector: 'app-complex-form',
  template: `
    <form [formGroup]="complexForm" (ngSubmit)="onSubmit()">
      <ng-container *ngFor="let section of uiStructure">
        <h2>{{ section.label }}</h2>
        <div [ngSwitch]="section.type">
          <div *ngSwitchCase="'commonHeader'" class="form-row">
            <ng-container *ngFor="let field of section.fields">
              <div [ngClass]="{'form-field': true, 'full-width': field.side === 'full'}">
                <label [for]="field.fldName">{{ field.label }}</label>
                <input 
                  [id]="field.fldName" 
                  [formControlName]="field.fldName" 
                  [type]="field.fieldType"
                  [required]="field.required"
                >
                <div *ngIf="field.errors && field.touched" class="error-message">
                  {{ getErrorMessage(field.errors) }}
                </div>
              </div>
            </ng-container>
          </div>
          <!-- Handle other section types (accordion, etc.) similarly -->
        </div>
      </ng-container>
      <button type="submit">Submit</button>
    </form>
  `,
  styles: [`
    .form-row { display: flex; flex-wrap: wrap; }
    .form-field { flex: 1 1 45%; margin: 10px; }
    .full-width { flex: 1 1 100%; }
    .error-message { color: red; font-size: 0.8em; }
  `]
})
export class ComplexFormComponent implements OnInit {
  complexForm: FormGroup;
  uiStructure: any[];

  constructor(
    private fb: FormBuilder,
    private formExtensionService: FormExtensionService
  ) {}

  ngOnInit() {
    this.initForm();
    this.updateUIStructure();
  }

  private initForm() {
    // Initialize your form and set layout as before
    // ...
  }

  private updateUIStructure() {
    this.uiStructure = this.formExtensionService.getUIReadView(this.complexForm, {
      includeErrors: true
    });
  }

  onSubmit() {
    if (this.complexForm.valid) {
      console.log('Form submitted:', this.formExtensionService.getRawFormGroupValue(this.complexForm));
    } else {
      this.formExtensionService.markAllAsTouched(this.complexForm);
      this.updateUIStructure(); // Refresh UI to show validation errors
    }
  }

  getErrorMessage(errors: any): string {
    // Logic to return appropriate error message based on error type
    if (errors.required) return 'This field is required';
    if (errors.email) return 'Invalid email format';
    // ... handle other error types
    return 'Invalid input';
  }
}


----------

  @Injectable({
  providedIn: 'root'
})
export class FormExtensionService {
  // ... other methods and properties

  getUIReadView(formGroup: FormGroup, options: UIReadViewOptions = {}): UIReadViewItem[] {
    const layout = this.formLayouts.get(formGroup);
    if (!layout) {
      console.warn('No layout found for this form group. Returning basic read view.');
      return this.getReadView(formGroup).map(item => this.convertToUIReadViewItem(item, formGroup.get(item.fldName)));
    }
    return this.applyLayoutToFormGroup(formGroup, layout, options);
  }

  private applyLayoutToFormGroup(formGroup: FormGroup, layout: FormLayout, options: UIReadViewOptions): UIReadViewItem[] {
    return layout.sections
      .filter(section => this.shouldIncludeSection(section, options))
      .flatMap(section => {
        const sectionFields = section.fields
          .filter(field => this.shouldIncludeField(field, options))
          .map(field => this.createUIReadViewItem(formGroup.get(field.name), field, options))
          .filter((item): item is UIReadViewItem => item !== null);

        if (sectionFields.length === 0) {
          return [];
        }

        const sectionItem: UIReadViewItem = {
          type: section.type,
          label: this.getSectionLabel(section),
          fldName: `section_${section.type}`,
          value: '', // Add a default value for sections
          editable: false,
          visible: true,
          expand: true,
          side: 'main', // Add a default side for sections
          seq: 0, // Add a default sequence for sections
          isValid: true,
          status: 'VALID',
          touched: false,
          dirty: false,
          fields: sectionFields
        };

        return [sectionItem, ...sectionFields];
      });
  }

  private createUIReadViewItem(control: AbstractControl | null, field: {name: string; side: string; seq: number}, options: UIReadViewOptions): UIReadViewItem | null {
    if (!control) return null;

    const extendedControl = control as ExtendedAbstractControl;
    const basicItem = this.createReadViewItem(control, field);
    if (!basicItem) return null;

    const item: UIReadViewItem = {
      ...basicItem,
      side: field.side,
      seq: field.seq,
      isValid: control.valid,
      status: control.status,
      touched: control.touched,
      dirty: control.dirty,
      type: extendedControl.fieldType || 'text', // Add a default type
      fields: [] // Initialize fields as an empty array
    };

    if (options.includeOldValues) {
      item.oldValue = extendedControl._oldValue;
    }

    if (options.includeErrors) {
      item.errors = control.errors;
    }

    return item;
  }

  private convertToUIReadViewItem(item: ReadViewItem, control: AbstractControl | null): UIReadViewItem {
    return {
      ...item,
      type: (control as ExtendedAbstractControl)?.fieldType || 'text',
      side: 'main',
      seq: 0,
      isValid: control?.valid ?? true,
      status: control?.status ?? 'VALID',
      touched: control?.touched ?? false,
      dirty: control?.dirty ?? false,
      fields: []
    };
  }

  // ... other methods
}

  
