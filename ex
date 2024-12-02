function transformObject(input: Record<string, any>, removeEmpty: boolean = false): Record<string, any> {
  const result: Record<string, any> = {};

  for (const [key, value] of Object.entries(input)) {
    let transformedValue;

    if (Array.isArray(value)) {
      if (value.length > 0 && value.every(item => typeof item === 'object' && item !== null)) {
        // Array of objects: Filter out empty objects if `removeEmpty` is true
        transformedValue = removeEmpty
          ? value.filter(item => Object.values(item).some(val => val !== null && val !== undefined && val !== ''))
          : value;
        // If filtered array is empty, skip this key
        if (removeEmpty && transformedValue.length === 0) continue;
      } else {
        // Array of primitives: Convert to pipe-separated string
        transformedValue = value.join('|');
      }
    } else if (typeof value === 'boolean') {
      // Boolean: Convert to "Y" or "N"
      transformedValue = value ? 'Y' : 'N';
    } else if (value === null || value === undefined || (typeof value === 'string' && value.trim() === '')) {
      // Remove empty values if `removeEmpty` is true
      if (removeEmpty) continue;
      transformedValue = value;
    } else {
      // Other types: Retain as-is
      transformedValue = value;
    }

    result[key] = transformedValue;
  }

  return result;
}



import { Injectable, signal, computed, Signal, effect, untracked } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, AbstractControl, ValidatorFn } from '@angular/forms';
import { ExtendedControlOptions, ExtendedAbstractControl, FieldItem, LayoutField, FormLayout, UIReadViewOptions } from './syn-form-extension.type';
import { CcoFormExtensionHelpers } from './syn-form-extension.helpers';
import { CCO_STATIC_COMPONENTS } from './syn-form-extension.constans';

@Injectable({
    providedIn: 'root'
})
export class SynFormExtensionService {
    private updateTrigger = signal(0);
    private readViewCache = new WeakMap<AbstractControl, Signal<FieldItem[]>>();
    private formLayouts = new Map<string, FormLayout>();
    private nextFormId = 0;
    private extendedPropertiesCache = new WeakMap<AbstractControl, ExtendedControlOptions>();
    private customValidatorsCache = new WeakMap<AbstractControl, ValidatorFn[]>();

    constructor(
        private fb: FormBuilder,
        private helpers: CcoFormExtensionHelpers
    ) {
        effect(() => {
            this.updateTrigger();
            this.clearCaches();
        });
    }

    setFormGroupLayout(formGroup: FormGroup, layout: FormLayout, uniqueId?: string): void {
        const formId = uniqueId || `form_${this.nextFormId++}`;
        const sortedLayout = this.helpers.sortFormLayout(layout);
        this.formLayouts.set(formId, sortedLayout);
        this.triggerUpdate();
    }

    getUIReadView(control: AbstractControl, options: UIReadViewOptions = {}, uniqueId?: string): FieldItem[] {
        let readView: FieldItem[];
        if (control instanceof FormGroup && uniqueId) {
            const layout = this.formLayouts.get(uniqueId);
            if (layout) {
                readView = this.helpers.applyLayoutToFormGroup(control, layout);
            } else {
                readView = this.helpers.getBasicReadViewForControl(control);
            }
        } else {
            readView = this.helpers.getBasicReadViewForControl(control);
        }
        return this.helpers.filterAndFormatReadView(readView, options);
    }

    getExtendedReadView(control: AbstractControl, uniqueId?: string): Signal<FieldItem[]> {
        const cacheKey = uniqueId ? `${control.id}_${uniqueId}` : control;
        if (!this.readViewCache.has(cacheKey)) {
            const readViewSignal = computed(() => {
                this.updateTrigger();
                return untracked(() => {
                    if (control instanceof FormGroup && uniqueId) {
                        const layout = this.formLayouts.get(uniqueId);
                        if (layout) {
                            return this.helpers.applyLayoutToFormGroup(control, layout);
                        }
                    }
                    return this.helpers.getBasicReadViewForControl(control);
                });
            });
            this.readViewCache.set(cacheKey, readViewSignal);
        }
        return this.readViewCache.get(cacheKey)!;
    }

    clearFormGroupLayout(uniqueId: string): void {
        if (uniqueId) {
            this.formLayouts.delete(uniqueId);
            this.triggerUpdate();
        }
    }

    private clearCaches(): void {
        this.readViewCache = new WeakMap();
        this.extendedPropertiesCache = new WeakMap();
    }

    // Rest of the service implementation remains the same...
}
