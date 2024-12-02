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
    private formGroupIds = new WeakMap<FormGroup, string>();
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
        let formId: string;
        
        if (uniqueId) {
            // Use the uniqueId directly
            formId = uniqueId;
        } else {
            // If no uniqueId, use existing or generate new
            formId = this.formGroupIds.get(formGroup) || `form_${this.nextFormId++}`;
        }

        // Update the mappings
        this.formGroupIds.set(formGroup, formId);
        const sortedLayout = this.helpers.sortFormLayout(layout);
        this.formLayouts.set(formId, sortedLayout);
        this.triggerUpdate();
    }

    getUIReadView(control: AbstractControl, options: UIReadViewOptions = {}, uniqueId?: string): FieldItem[] {
        let readView: FieldItem[];
        if (control instanceof FormGroup) {
            // First try to get layout using provided uniqueId
            let layout: FormLayout | undefined;
            if (uniqueId) {
                layout = this.formLayouts.get(uniqueId);
            }
            
            // If no layout found and no uniqueId provided, try FormGroup's stored ID
            if (!layout && !uniqueId) {
                const formId = this.formGroupIds.get(control);
                layout = formId ? this.formLayouts.get(formId) : undefined;
            }

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

    clearFormGroupLayout(formGroup: FormGroup, uniqueId?: string): void {
        if (uniqueId) {
            this.formLayouts.delete(uniqueId);
        } else {
            const formId = this.formGroupIds.get(formGroup);
            if (formId) {
                this.formLayouts.delete(formId);
                this.formGroupIds.delete(formGroup);
            }
        }
        this.triggerUpdate();
    }

    private clearCaches(): void {
        this.readViewCache = new WeakMap();
        this.extendedPropertiesCache = new WeakMap();
        // Don't clear formLayouts or formGroupIds as they need to persist
    }

    // Rest of the service implementation remains the same...
}
