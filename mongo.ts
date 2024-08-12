// src/app/models/profile.model.ts
export interface Profile {
  profileIdentifier: string;
  roles: string[];
  partyDetails: PartyDetails;
}

export interface PartyDetails {
  personalInfo: PersonalInfo;
  contact: Contact;
  financialInfo: FinancialInfo;
  professional: Professional;
  relationships: Relationship[];
}

export interface PersonalInfo {
  nameInfo: {
    firstName: string;
    lastName: string;
  };
  ssn: {
    type: string;
    number: string;
  };
  age: number;
  dob: string;
  married: boolean;
  usa: string;
  identification: Array<{
    type: string;
    number: string;
  }>;
}

// Other interfaces (Contact, FinancialInfo, Professional, Relationship) would be defined here

// src/app/services/profile-builder.service.ts
import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { HttpClient } from '@angular/common/http';
import { Observable, forkJoin } from 'rxjs';
import { map } from 'rxjs/operators';
import { Profile } from '../models/profile.model';

@Injectable({
  providedIn: 'root'
})
export class ProfileBuilderService {
  constructor(
    private fb: FormBuilder,
    private http: HttpClient
  ) {}

  createProfilesForm(): FormGroup {
    return this.fb.group({
      profiles: this.fb.array([])
    });
  }

  createProfileForm(): FormGroup {
    return this.fb.group({
      profileIdentifier: [''],
      roles: this.fb.array([]),
      partyDetails: this.createPartyDetailsForm()
    });
  }

  private createPartyDetailsForm(): FormGroup {
    return this.fb.group({
      personalInfo: this.createPersonalInfoForm(),
      contact: this.fb.group({}),  // Implement full structure as needed
      financialInfo: this.fb.group({}),  // Implement full structure as needed
      professional: this.fb.group({}),  // Implement full structure as needed
      relationships: this.fb.array([])
    });
  }

  private createPersonalInfoForm(): FormGroup {
    return this.fb.group({
      nameInfo: this.fb.group({
        firstName: ['', [Validators.required, Validators.minLength(2)]],
        lastName: ['', [Validators.required, Validators.minLength(2)]]
      }),
      ssn: this.fb.group({
        type: ['', Validators.required],
        number: ['', [Validators.required, Validators.pattern(/^\d{3}-\d{2}-\d{4}$/)]]
      }),
      age: [null, [Validators.required, Validators.min(0), Validators.max(120)]],
      dob: ['', Validators.required],
      married: [false],
      usa: ['N'],
      identification: this.fb.array([])
    });
  }

  loadProfileData(partyId: string): Observable<{ profiles: Profile[], metadata: any }> {
  const profilesObservable = partyId ? this.getProfilesByPartyId(partyId) : of([]);

  return forkJoin({
    profiles: profilesObservable,
    metadata: this.getMetadata(partyId)
  });
}


  private getProfilesByPartyId(partyId: string): Observable<Profile[]> {
    return this.http.get<Profile[]>(`/api/profiles/${partyId}`);
  }

  private getMetadata(partyId: string): Observable<any> {
    return this.http.get<any>(`/api/metadata/${partyId}`);
  }

  applyDataToForm(profilesForm: FormGroup, profiles: Profile[], metadata: any): void {
    const profilesArray = profilesForm.get('profiles') as FormArray;
    profilesArray.clear();

    profiles.forEach(profile => {
      const profileForm = this.createProfileForm();
      this.patchProfileForm(profileForm, profile);
      this.applyMetadataToProfile(profileForm, metadata);
      profilesArray.push(profileForm);
    });
  }

  private patchProfileForm(profileForm: FormGroup, profile: Profile): void {
    profileForm.patchValue(profile);
    this.patchFormArray(profileForm.get('roles') as FormArray, profile.roles);
    // Patch other nested structures as needed
  }

  private patchFormArray(formArray: FormArray, data: any[]): void {
    formArray.clear();
    data.forEach(item => formArray.push(this.fb.control(item)));
  }

  private applyMetadataToProfile(profileForm: FormGroup, metadata: any): void {
    Object.keys(metadata).forEach(key => {
      const control = profileForm.get(key);
      if (control && metadata[key]) {
        if (metadata[key].visible === false) {
          control.disable();
        }
        if (metadata[key].editable === false) {
          control.disable();
        }
        // Apply other metadata properties as needed
      }
    });
  }

  addProfile(profilesForm: FormGroup): void {
    const profilesArray = profilesForm.get('profiles') as FormArray;
    profilesArray.push(this.createProfileForm());
  }

  removeProfile(profilesForm: FormGroup, index: number): void {
    const profilesArray = profilesForm.get('profiles') as FormArray;
    profilesArray.removeAt(index);
  }

  saveProfiles(profiles: Profile[]): Observable<Profile[]> {
    return this.http.post<Profile[]>('/api/profiles', profiles);
  }
}

// src/app/containers/profile-builder/profile-builder.component.ts
import { Component, OnInit, Input } from '@angular/core';
import { FormGroup, FormArray } from '@angular/forms';
import { ProfileBuilderService } from '../../services/profile-builder.service';
import { Profile } from '../../models/profile.model';

@Component({
  selector: 'app-profile-builder',
  templateUrl: './profile-builder.component.html'
})
export class ProfileBuilderComponent implements OnInit {
  @Input() partyId?: string;
  
  profilesForm: FormGroup;
  metadata: any = {};

  constructor(private profileBuilderService: ProfileBuilderService) {}

  ngOnInit() {
    this.createForm();
    if (this.partyId) {
      this.loadData();
    } else {
      this.addNewProfile();
    }
  }

  private createForm() {
    this.profilesForm = this.profileBuilderService.createProfilesForm();
  }

  private loadData() {
    this.profileBuilderService.loadProfileData(this.partyId).subscribe(
      ({ profiles, metadata }) => {
        this.metadata = metadata;
        this.profileBuilderService.applyDataToForm(this.profilesForm, profiles, metadata);
      },
      error => console.error('Error loading profile data:', error)
    );
  }

  addNewProfile() {
    this.profileBuilderService.addProfile(this.profilesForm);
  }

  removeProfile(index: number) {
    this.profileBuilderService.removeProfile(this.profilesForm, index);
  }

  onSubmit() {
    if (this.profilesForm.valid) {
      const profilesData: Profile[] = this.profilesForm.value.profiles;
      this.profileBuilderService.saveProfiles(profilesData).subscribe(
        result => console.log('Profiles saved successfully', result),
        error => console.error('Error saving profiles:', error)
      );
    } else {
      this.markFormGroupTouched(this.profilesForm);
    }
  }

  private markFormGroupTouched(formGroup: FormGroup | FormArray) {
    Object.values(formGroup.controls).forEach(control => {
      if (control instanceof FormGroup || control instanceof FormArray) {
        this.markFormGroupTouched(control);
      } else {
        control.markAsTouched();
      }
    });
  }
}

// src/app/components/personal-info/personal-info.component.ts
import { Component, Input } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-personal-info',
  templateUrl: './personal-info.component.html'
})
export class PersonalInfoComponent {
  @Input() personalInfoForm: FormGroup;
  @Input() metadata: any;

  addIdentification() {
    // Implementation for adding identification
  }

  removeIdentification(index: number) {
    // Implementation for removing identification
  }
}

// src/app/containers/profile-builder/profile-builder.component.html
<form [formGroup]="profilesForm" (ngSubmit)="onSubmit()">
  <div formArrayName="profiles">
    <div *ngFor="let profileForm of profilesForm.get('profiles')['controls']; let i = index">
      <div [formGroupName]="i">
        <h3>Profile {{ i + 1 }}</h3>
        
        <div formGroupName="partyDetails">
          <app-personal-info 
            [personalInfoForm]="profileForm.get('partyDetails.personalInfo')"
            [metadata]="metadata.personalInfo">
          </app-personal-info>
          
          <!-- Other sections (Contact, Financial, Professional, etc.) would go here -->
        </div>
        
      </div>
      <button type="button" (click)="removeProfile(i)">Remove Profile</button>
    </div>
  </div>
  <button type="button" (click)="addNewProfile()">Add New Profile</button>
  <button type="submit">Submit</button>
</form>

// src/app/components/personal-info/personal-info.component.html
<div [formGroup]="personalInfoForm">
  <div formGroupName="nameInfo">
    <label>
      First Name:
      <input formControlName="firstName">
    </label>
    <label>
      Last Name:
      <input formControlName="lastName">
    </label>
  </div>
  
  <div formGroupName="ssn">
    <label>
      SSN Type:
      <input formControlName="type">
    </label>
    <label>
      SSN Number:
      <input formControlName="number">
    </label>
  </div>
  
  <label>
    Age:
    <input formControlName="age" type="number">
  </label>
  
  <label>
    Date of Birth:
    <input formControlName="dob" type="date">
  </label>
  
  <label>
    Married:
    <input formControlName="married" type="checkbox">
  </label>
  
  <label>
    USA:
    <input formControlName="usa">
  </label>
  
  <div formArrayName="identification">
    <div *ngFor="let id of personalInfoForm.get('identification')['controls']; let i = index">
      <div [formGroupName]="i">
        <label>
          ID Type:
          <input formControlName="type">
        </label>
        <label>
          ID Number:
          <input formControlName="number">
        </label>
        <button type="button" (click)="removeIdentification(i)">Remove</button>
      </div>
    </div>
    <button type="button" (click)="addIdentification()">Add Identification</button>
  </div>
</div>
