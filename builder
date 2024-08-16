// widget-config.ts
import { Type } from '@angular/core';
import { Signal, signal } from '@angular/core';

export interface WidgetState {
  hasError: boolean;
  visited: boolean;
  acknowledged: boolean;
  active: boolean;
  status: string;
}

export interface WidgetConfig {
  name: string;
  label: string;
  component: () => Promise<Type<any>>;
  visible: boolean;
  state: Signal<WidgetState>;
}

export const WIDGET_CONFIG: WidgetConfig[] = [
  {
    name: 'personal',
    label: 'Personal Info',
    component: () => import('./personal-info.component').then(m => m.PersonalInfoComponent),
    visible: true,
    state: signal<WidgetState>({ hasError: false, visited: false, acknowledged: false, active: false, status: 'pending' })
  },
  {
    name: 'financial',
    label: 'Financial Info',
    component: () => import('./financial-info.component').then(m => m.FinancialInfoComponent),
    visible: true,
    state: signal<WidgetState>({ hasError: false, visited: false, acknowledged: false, active: false, status: 'pending' })
  },
  // Add more widgets as needed
];

// profile-builder.service.ts
import { Injectable, signal, computed, inject, effect } from '@angular/core';
import { FormBuilder, FormGroup, FormArray } from '@angular/forms';
import { HttpClient } from '@angular/common/http';
import { WIDGET_CONFIG, WidgetConfig, WidgetState } from './widget-config';

@Injectable({ providedIn: 'root' })
export class ProfileBuilderService {
  private fb = inject(FormBuilder);
  private http = inject(HttpClient);

  private profileForm = signal<FormGroup>(this.fb.group({
    profiles: this.fb.array([])
  }));

  private activeProfileIndex = signal<number>(0);
  private widgetStates = signal<Record<string, WidgetState>>(
    Object.fromEntries(
      WIDGET_CONFIG.map(widget => [
        widget.name,
        { hasError: false, visited: false, acknowledged: false, active: false, status: 'pending' }
      ])
    )
  );

  private activeWidgetName = signal<string | null>(null);
  private loadedWidgets = signal<Set<string>>(new Set());

  constructor() {
    effect(() => {
      const activeProfile = this.getActiveProfile();
      if (activeProfile) {
        this.loadInitialWidget();
      }
    });
  }

  getProfileForm(): FormGroup {
    return this.profileForm();
  }

  getActiveProfile(): FormGroup | null {
    const profiles = this.profileForm().get('profiles') as FormArray;
    return profiles.at(this.activeProfileIndex()) as FormGroup;
  }

  getVisibleWidgets = computed(() => 
    WIDGET_CONFIG.filter(widget => widget.visible).map(widget => ({
      ...widget,
      state: computed(() => this.widgetStates()[widget.name])
    }))
  );

  activeWidget = computed(() => {
    const name = this.activeWidgetName();
    return name ? this.getVisibleWidgets().find(w => w.name === name) : null;
  });

  async createOrEditProfile(partyId?: string) {
    let profileData: any = {};
    const metadataData = await this.fetchMetadata();

    if (partyId) {
      profileData = await this.fetchPartyData(partyId);
    }

    const profileFormGroup = this.createProfileFormGroup(profileData, metadataData);
    (this.profileForm().get('profiles') as FormArray).push(profileFormGroup);
    this.setActiveProfile((this.profileForm().get('profiles') as FormArray).length - 1);
    this.resetWidgetStates();
    this.loadInitialWidget();
  }

  private createProfileFormGroup(profileData: any, metadataData: any): FormGroup {
    const profileGroup = this.fb.group({});

    this.getVisibleWidgets().forEach(widget => {
      profileGroup.addControl(widget.name, this.fb.group({}));
    });

    if (profileData) {
      profileGroup.patchValue(profileData);
    }

    return profileGroup;
  }

  private async fetchMetadata() {
    return this.http.get('metadata-api-url').toPromise();
  }

  private async fetchPartyData(partyId: string) {
    return this.http.get(`party-api-url/${partyId}`).toPromise();
  }

  setActiveProfile(index: number) {
    this.activeProfileIndex.set(index);
    this.resetWidgetStates();
  }

  setActiveWidget(widgetName: string) {
    this.activeWidgetName.set(widgetName);
    this.updateWidgetState(widgetName, { active: true, visited: true, status: 'in-progress' });
  }

  updateWidgetState(widgetName: string, update: Partial<WidgetState>) {
    this.widgetStates.update(states => ({
      ...states,
      [widgetName]: { ...states[widgetName], ...update }
    }));
  }

  setWidgetLoaded(widgetName: string) {
    this.loadedWidgets.update(loaded => new Set(loaded).add(widgetName));
    this.updateWidgetState(widgetName, { status: 'loaded' });
  }

  isWidgetLoaded(widgetName: string) {
    return computed(() => this.loadedWidgets().has(widgetName));
  }

  acknowledgeWidget(widgetName: string) {
    this.updateWidgetState(widgetName, { acknowledged: true, status: 'completed' });
  }

  resetWidgetStates() {
    this.widgetStates.update(() => 
      Object.fromEntries(
        WIDGET_CONFIG.map(widget => [
          widget.name,
          { hasError: false, visited: false, acknowledged: false, active: false, status: 'pending' }
        ])
      )
    );
    this.loadedWidgets.set(new Set());
  }

  validateAllWidgets(): boolean {
    const activeProfile = this.getActiveProfile();
    let isValid = true;
    this.getVisibleWidgets().forEach(widget => {
      const widgetForm = activeProfile?.get(widget.name);
      const widgetValid = widgetForm?.valid ?? false;
      this.updateWidgetState(widget.name, { 
        hasError: !widgetValid, 
        status: widgetValid ? 'completed' : 'error' 
      });
      isValid = isValid && widgetValid;
    });
    return isValid;
  }

  // Helper Methods
  getWidgetByName(name: string) {
    return this.getVisibleWidgets().find(widget => widget.name === name);
  }

  isProfileComplete(): boolean {
    return this.getVisibleWidgets().every(widget => 
      this.widgetStates()[widget.name].status === 'completed'
    );
  }

  getCompletionPercentage(): number {
    const totalWidgets = this.getVisibleWidgets().length;
    const completedWidgets = this.getVisibleWidgets().filter(widget => 
      this.widgetStates()[widget.name].status === 'completed'
    ).length;
    return (completedWidgets / totalWidgets) * 100;
  }

  getNextIncompleteWidget(): string | null {
    const incompleteWidget = this.getVisibleWidgets().find(widget => 
      this.widgetStates()[widget.name].status !== 'completed'
    );
    return incompleteWidget ? incompleteWidget.name : null;
  }

  loadInitialWidget() {
    const firstWidget = this.getVisibleWidgets()[0];
    if (firstWidget) {
      this.setActiveWidget(firstWidget.name);
    }
  }
}

// party-builder.component.ts
import { Component, ViewContainerRef, ViewChild, effect, inject, Injector } from '@angular/core';
import { AsyncPipe, NgFor, NgIf } from '@angular/common';
import { ProfileBuilderService } from './profile-builder.service';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-party-builder',
  standalone: true,
  imports: [AsyncPipe, NgFor, NgIf],
  template: `
    <div class="party-builder">
      <app-left-menu></app-left-menu>
      <div class="widget-container">
        <ng-container *ngFor="let widget of profileService.getVisibleWidgets()">
          <ng-container *ngIf="profileService.isWidgetLoaded(widget.name)() || widget.state().active">
            <ng-container #widgetContainer></ng-container>
          </ng-container>
        </ng-container>
      </div>
    </div>
    <button (click)="createNewProfile()">Create New Profile</button>
    <button (click)="editExistingProfile('some-party-id')">Edit Existing Profile</button>
    <button (click)="validateAll()">Validate All</button>
    <button (click)="loadAllWidgets()">Load All Widgets</button>
    <button (click)="loadNextIncompleteWidget()">Next Incomplete Widget</button>
  `
})
export class PartyBuilderComponent {
  @ViewChild('widgetContainer', { read: ViewContainerRef, static: false, emitDistinctChangesOnly: false })
  widgetContainers!: ViewContainerRef[];

  profileService = inject(ProfileBuilderService);
  private injector = inject(Injector);

  constructor() {
    effect(() => {
      const activeWidget = this.profileService.activeWidget();
      if (activeWidget) {
        this.loadComponent(activeWidget.name);
      }
    }, { injector: this.injector });
  }

  async loadComponent(widgetName: string) {
    if (!this.profileService.isWidgetLoaded(widgetName)()) {
      const widgetConfig = this.profileService.getWidgetByName(widgetName);
      if (widgetConfig) {
        const component = await widgetConfig.component();
        const containerIndex = this.profileService.getVisibleWidgets().findIndex(w => w.name === widgetName);
        if (containerIndex !== -1 && this.widgetContainers[containerIndex]) {
          const componentRef = this.widgetContainers[containerIndex].createComponent(component);
          const activeProfile = this.profileService.getActiveProfile();
          if (activeProfile) {
            const widgetForm = activeProfile.get(widgetName) as FormGroup;
            if (componentRef.instance.setFormGroup) {
              componentRef.instance.setFormGroup(widgetForm);
            }
          }
          this.profileService.setWidgetLoaded(widgetName);
        }
      }
    }
  }

  async loadAllWidgets() {
    for (const widget of this.profileService.getVisibleWidgets()) {
      await this.loadComponent(widget.name);
    }
  }

  async createNewProfile() {
    await this.profileService.createOrEditProfile();
  }

  async editExistingProfile(partyId: string) {
    await this.profileService.createOrEditProfile(partyId);
  }

  validateAll() {
    const isValid = this.profileService.validateAllWidgets();
    console.log(isValid ? 'All widgets are valid' : 'There are validation errors');
    console.log('Completion percentage:', this.profileService.getCompletionPercentage());
  }

  loadNextIncompleteWidget() {
    const nextWidgetName = this.profileService.getNextIncompleteWidget();
    if (nextWidgetName) {
      this.profileService.setActiveWidget(nextWidgetName);
    } else {
      console.log('All widgets are complete!');
    }
  }
}

// left-menu.component.ts
import { Component, inject } from '@angular/core';
import { NgFor, NgClass } from '@angular/common';
import { ProfileBuilderService } from './profile-builder.service';

@Component({
  selector: 'app-left-menu',
  standalone: true,
  imports: [NgFor, NgClass],
  template: `
    <nav>
      <ul>
        <li *ngFor="let widget of profileService.getVisibleWidgets()">
          <a (click)="setActiveWidget(widget.name)" 
             [ngClass]="{
               'active': widget.state().active,
               'visited': widget.state().visited,
               'error': widget.state().hasError,
               'acknowledged': widget.state().acknowledged,
               [widget.state().status]: true
             }">
            {{ widget.label }}
          </a>
        </li>
      </ul>
    </nav>
    <div>Completion: {{ profileService.getCompletionPercentage() | number:'1.0-0' }}%</div>
  `
})
export class LeftMenuComponent {
  profileService = inject(ProfileBuilderService);

  setActiveWidget(widgetName: string) {
    this.profileService.setActiveWidget(widgetName);
  }
}

// personal-info.component.ts
import { Component, OnInit, OnDestroy, inject } from '@angular/core';
import { FormGroup, FormBuilder, Validators, ReactiveFormsModule } from '@angular/forms';
import { Subscription } from 'rxjs';
import { ProfileBuilderService } from './profile-builder.service';

@Component({
  selector: 'app-personal-info',
  standalone: true,
  imports: [ReactiveFormsModule],
  template: `
    <form *ngIf="form" [formGroup]="form">
      <input formControlName="firstName" placeholder="First Name">
      <input formControlName="lastName" placeholder="Last Name">
      <input formControlName="dateOfBirth" placeholder="Date of Birth">
    </form>
    <button (click)="acknowledge()">Acknowledge</button>
  `
})
export class PersonalInfoComponent implements OnInit, OnDestroy {
  form: FormGroup;
  private subscription: Subscription;
  private fb = inject(FormBuilder);
  private profileService = inject(ProfileBuilderService);

  ngOnInit() {
    this.subscription = this.form.statusChanges.subscribe(() => {
      this.updateValidationStatus();
    });
  }

  ngOnDestroy() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }
  }

  setFormGroup(form: FormGroup) {
    this.form = form;
    if (Object.keys(this.form.controls).length === 0) {
      this.form.addControl('firstName', this.fb.control('', Validators.required));
      this.form.addControl('lastName', this.fb.control('', Validators.required));
      this.form.addControl('dateOfBirth', this.fb.control('', Validators.required));
    }
  }

  private updateValidationStatus() {
    this.profileService.updateWidgetState('personal', { 
      hasError: !this.form.valid, 
      status: this.form.valid ? 'in-progress' : 'error' 
    });
  }

  acknowledge() {
    this.profileService.acknowledgeWidget('personal');
  }
