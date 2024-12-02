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
