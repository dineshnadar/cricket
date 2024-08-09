import { Component, inject, effect } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { signalStore, withComputed, withMethods, withState } from '@ngrx/signals';
import { setAllProps } from '@ngrx/signals/operators';
import { computed } from '@angular/core';

// Define the form state interface
interface FormState {
  firstName: string;
  lastName: string;
  email: string;
  sex: string;
  favoriteColor: string;
  employed: boolean;
  notes: string;
}

// Create a signal store for the form
const useFormStore = signalStore(
  { providedIn: 'root' },
  withState<FormState>({
    firstName: '',
    lastName: '',
    email: '',
    sex: '',
    favoriteColor: '',
    employed: false,
    notes: ''
  }),
  withComputed((state) => ({
    isValid: computed(() => 
      state.firstName().length > 0 && 
      state.lastName().length > 0 && 
      state.email().length > 0
    ),
    isDirty: computed(() => 
      Object.values(state).some(field => field() !== '')
    )
  })),
  withMethods((store) => ({
    updateField(field: keyof FormState, value: any) {
      store[field].set(value);
    },
    resetForm() {
      store.firstName.set('');
      store.lastName.set('');
      store.email.set('');
      store.sex.set('');
      store.favoriteColor.set('');
      store.employed.set(false);
      store.notes.set('');
    }
  }))
);

@Component({
  selector: 'app-simple-form',
  standalone: true,
  imports: [CommonModule, ReactiveFormsModule],
  template: `
    <form [formGroup]="form" (ngSubmit)="onSubmit()">
      <div>
        <label for="firstName">First Name:</label>
        <input id="firstName" formControlName="firstName">
      </div>
      <div>
        <label for="lastName">Last Name:</label>
        <input id="lastName" formControlName="lastName">
      </div>
      <div>
        <label for="email">Email:</label>
        <input id="email" formControlName="email" type="email">
      </div>
      <div>
        <label for="sex">Sex:</label>
        <select id="sex" formControlName="sex">
          <option value="male">Male</option>
          <option value="female">Female</option>
          <option value="other">Other</option>
        </select>
      </div>
      <div>
        <label for="favoriteColor">Favorite Color:</label>
        <input id="favoriteColor" formControlName="favoriteColor">
      </div>
      <div>
        <label for="employed">Employed:</label>
        <input id="employed" formControlName="employed" type="checkbox">
      </div>
      <div>
        <label for="notes">Notes:</label>
        <textarea id="notes" formControlName="notes"></textarea>
      </div>
      <button type="submit" [disabled]="!formStore.isValid()">Submit</button>
      <button type="button" (click)="resetForm()">Reset</button>
    </form>
    <div>Form is valid: {{ formStore.isValid() }}</div>
    <div>Form is dirty: {{ formStore.isDirty() }}</div>
  `
})
export class SimpleFormComponent {
  private fb = inject(FormBuilder);
  formStore = inject(useFormStore);

  form = this.fb.group({
    firstName: ['', Validators.required],
    lastName: ['', Validators.required],
    email: ['', [Validators.required, Validators.email]],
    sex: [''],
    favoriteColor: [''],
    employed: [false],
    notes: ['']
  });

  constructor() {
    effect(() => {
      const formValue = this.form.value;
      Object.keys(formValue).forEach(key => {
        this.formStore.updateField(key as keyof FormState, formValue[key as keyof FormState]);
      });
    });
  }

  onSubmit() {
    if (this.form.valid) {
      console.log('Form submitted:', this.form.value);
      // Here you would typically send the form data to a server
    }
  }

  resetForm() {
    this.form.reset();
    this.formStore.resetForm();
  }
}
