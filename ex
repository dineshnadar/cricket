import { Injectable, signal, computed, Signal, effect, untraced } from '@angular/core';
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
    private formLayouts = new Map<string, FormLayout>();  // Changed to Map with string keys
    private formGroupIds = new WeakMap<FormGroup, string>();  // Added to track form group IDs
    private nextFormId = 0;  // Counter for generating unique form IDs
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

    // New method to generate or retrieve form ID
    private getFormGroupId(formGroup: FormGroup): string {
        let formId = this.formGroupIds.get(formGroup);
        if (!formId) {
            formId = `form_${this.nextFormId++}`;
            this.formGroupIds.set(formGroup, formId);
        }
        return formId;
    }

    // Updated setFormGroupLayout method
    setFormGroupLayout(formGroup: FormGroup, layout: FormLayout): void {
        const formId = this.getFormGroupId(formGroup);
        const sortedLayout = this.helpers.sortFormLayout(layout);
        this.formLayouts.set(formId, sortedLayout);
        this.triggerUpdate();
    }

    // Updated getExtendedReadView method
    getExtendedReadView(control: AbstractControl): Signal<FieldItem[]> {
        if (!this.readViewCache.has(control)) {
            const readViewSignal = computed(() => {
                this.updateTrigger();
                return untracked(() => {
                    if (control instanceof FormGroup) {
                        const formId = this.getFormGroupId(control);
                        const layout = this.formLayouts.get(formId);
                        if (layout) {
                            return this.helpers.applyLayoutToFormGroup(control, layout);
                        }
                    }
                    return this.helpers.getBasicReadViewForControl(control);
                });
            });
            this.readViewCache.set(control, readViewSignal);
        }
        return this.readViewCache.get(control)!;
    }

    // Updated getUIReadView method
    getUIReadView(control: AbstractControl, options: UIReadViewOptions = {}): FieldItem[] {
        let readView: FieldItem[];
        if (control instanceof FormGroup) {
            const formId = this.getFormGroupId(control);
            const layout = this.formLayouts.get(formId);
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

    // Updated clearCaches method
    private clearCaches(): void {
        this.readViewCache = new WeakMap();
        this.extendedPropertiesCache = new WeakMap();
        // Note: We don't clear formLayouts or formGroupIds here as they need to persist
    }

    // Method to clear layout for a specific form group
    clearFormGroupLayout(formGroup: FormGroup): void {
        const formId = this.formGroupIds.get(formGroup);
        if (formId) {
            this.formLayouts.delete(formId);
            this.formGroupIds.delete(formGroup);
            this.triggerUpdate();
        }
    }

    // Rest of the service implementation remains the same...
}
