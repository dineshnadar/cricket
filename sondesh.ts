import { Injectable, signal, computed, Signal } from '@angular/core';
import { FormGroup, FormArray, AbstractControl } from '@angular/forms';

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
}

interface LayoutSection {
  type: 'leftHeader' | 'leftRightHeader' | 'commonHeader' | 'accordion' | 'accordionHeader';
  leftHeader?: string;
  rightHeader?: string;
  commonHeader?: string;
  accordionHeader?: string;
  fields: Array<{name: string; side: 'left' | 'right'; seq: number}>;
}

interface FormLayout {
  type: 'standard' | 'noRightHeader' | 'multipleSection';
  sections: LayoutSection[];
}

interface ExtendedReadViewItem extends ReadViewItem {
  layout?: FormLayout;
}

@Injectable({
  providedIn: 'root'
})
export class FormExtensionService {
  private updateTrigger = signal(0);
  private readViewCache = new WeakMap<AbstractControl, Signal<ExtendedReadViewItem[]>>();
  private formLayouts = new WeakMap<FormGroup, FormLayout>();

  extendControl(control: AbstractControl, options: ExtendedControlOptions) {
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
      this.triggerUpdate();
    }
  }

  private extendNestedControls(control: FormGroup | FormArray) {
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
  ) {
    const extendedControl = control as ExtendedAbstractControl;
    if (property === 'regex') {
      if (!this.areRegexArraysEqual(extendedControl[property], value as string[])) {
        extendedControl[property] = value as any;
        this.triggerUpdate();
      }
    } else if (extendedControl[property] !== value) {
      extendedControl[property] = value as any;
      this.triggerUpdate();
    }
  }

  private areRegexArraysEqual(arr1?: string[], arr2?: string[]): boolean {
    if (!arr1 && !arr2) return true;
    if (!arr1 || !arr2) return false;
    if (arr1.length !== arr2.length) return false;
    return arr1.every((regex, index) => regex === arr2[index]);
  }

  setFormGroupLayout(formGroup: FormGroup, layout: FormLayout) {
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

  getExtendedReadView(control: AbstractControl): Signal<ExtendedReadViewItem[]> {
    if (!this.readViewCache.has(control)) {
      const readViewSignal = computed(() => {
        this.updateTrigger(); // Depend on the update trigger
        return this.getExtendedReadViewForControl(control);
      });
      this.readViewCache.set(control, readViewSignal);
    }
    return this.readViewCache.get(control)!;
  }

  private getExtendedReadViewForControl(control: AbstractControl): ExtendedReadViewItem[] {
    if (control instanceof FormGroup) {
      const layout = this.formLayouts.get(control);
      if (layout) {
        return this.applyLayoutToFormGroup(control, layout);
      }
    }
    return this.getBasicReadViewForControl(control);
  }

  private applyLayoutToFormGroup(formGroup: FormGroup, layout: FormLayout): ExtendedReadViewItem[] {
    return layout.sections.map(section => ({
      label: this.getSectionLabel(section),
      fldName: `section_${section.type}`,
      value: null,
      editable: false,
      visible: true,
      expand: true,
      children: section.fields
        .map(field => this.createReadViewItem(formGroup.get(field.name)))
        .filter((item): item is ExtendedReadViewItem => item !== null),
      layout: { type: layout.type, sections: [section] }
    }));
  }

  private getBasicReadViewForControl(control: AbstractControl): ExtendedReadViewItem[] {
    const item = this.createReadViewItem(control);
    if (!item) return [];

    if (control instanceof FormGroup || control instanceof FormArray) {
      item.children = Object.entries(control.controls)
        .map(([key, childControl]) => this.createReadViewItem(childControl))
        .filter((child): child is ExtendedReadViewItem => child !== null);
    }

    return [item];
  }

  private createReadViewItem(control: AbstractControl | null): ExtendedReadViewItem | null {
    if (!control) return null;
    const extendedControl = control as ExtendedAbstractControl;
    const rawValue = control.value;
    const transformedValue = this.transformValue(rawValue, extendedControl);

    const item: ExtendedReadViewItem = {
      label: extendedControl.label || '',
      fldName: extendedControl.fldName || '',
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

  registerCustomComputation(control: AbstractControl, computation: (control: AbstractControl, form: FormGroup) => any) {
    (control as ExtendedAbstractControl)._customComputation = computation;
    this.triggerUpdate();
  }

  applyDefaultValues(control: AbstractControl, storeOldValue: boolean = false) {
    this.applyDefaultValuesRecursive(control, storeOldValue);
    this.triggerUpdate();
  }

  private applyDefaultValuesRecursive(control: AbstractControl, storeOldValue: boolean) {
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

  private setFormGroupValue(group: FormGroup, value: any) {
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
    const extendedControl = control as ExtendedAbstractControl;
    return {
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
  }

  batchUpdates(updates: () => void) {
    updates();
    this.triggerUpdate();
  }

  private triggerUpdate() {
    this.updateTrigger.update(v => v + 1);
    this.readViewCache = new WeakMap(); // Clear cache on update
  }
}

// user-profile.component.ts

import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { FormExtensionService, FormLayout } from './form-extension.service';

@Component({
  selector: 'app-user-profile',
  template: `
    <form [formGroup]="profileForm">
      <!-- Form fields would be rendered here based on the layout -->
    </form>
    <h2>Extended Read View</h2>
    <pre>{{ extendedReadView() | json }}</pre>
  `
})
export class UserProfileComponent implements OnInit {
  profileForm: FormGroup;
  extendedReadView = this.formExtensionService.getExtendedReadView(this.profileForm);

  constructor(
    private fb: FormBuilder,
    private formExtensionService: FormExtensionService
  ) {}

  ngOnInit() {
    this.initForm();
    this.extendFormControls();
    this.setFormLayout();
    this.registerCustomComputations();
    this.formExtensionService.applyDefaultValues(this.profileForm, true);
  }

  private initForm() {
    this.profileForm = this.fb.group({
      personalInfo: this.fb.group({
        firstName: ['', Validators.required],
        lastName: ['', Validators.required],
      }),
      email: ['', [Validators.required, Validators.email]],
      phone: [''],
      dateOfBirth: ['', Validators.required],
      gender: [''],
      address: this.fb.group({
        street: [''],
        city: [''],
        state: [''],
        zipCode: [''],
        country: ['']
      }),
      employment: this.fb.group({
        currentlyEmployed: [false],
        employer: [''],
        position: [''],
        startDate: [''],
        salary: ['']
      }),
      education: this.fb.array([]),
      skills: this.fb.array([]),
      preferences: this.fb.group({
        newsletter: [false],
        theme:
