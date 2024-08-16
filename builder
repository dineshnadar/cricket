// widget-config.ts
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
  component: () => Promise<any>;
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
  }
  // Add more widgets as needed
];

// profile-builder.service.ts
import { Injectable, signal, computed, inject } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { HttpClient } from '@angular/common/http';
import { WIDGET_CONFIG, WidgetConfig, WidgetState } from './widget-config';

@Injectable({ providedIn: 'root' })
export class ProfileBuilderService {
  private fb = inject(FormBuilder);
  private http = inject(HttpClient);

  private profiles = signal<FormGroup[]>([]);
  private activeProfileIndex = signal<number | null>(null);
  private profileWidgetStates = signal<Array<Record<string, Signal<WidgetState>>>>([]);
  private activeWidgetName = signal<string | null>(null);

  addNewProfile() {
    const newProfileForm = this.createProfileFormGroup();
    this.profiles.update(profiles => [...profiles, newProfileForm]);
    
    const newProfileWidgetStates = Object.fromEntries(
      WIDGET_CONFIG.map(widget => [
        widget.name, 
        signal<WidgetState>({ ...widget.state() })
      ])
    );
    this.profileWidgetStates.update(states => [...states, newProfileWidgetStates]);
    
    this.setActiveProfile(this.profiles().length - 1);
  }

  setActiveProfile(index: number) {
    if (index >= 0 && index < this.profiles().length) {
      this.activeProfileIndex.set(index);
      this.resetWidgetStates();
      this.loadInitialWidget();
    } else {
      console.error('Invalid profile index');
    }
  }

  getActiveProfile(): FormGroup | null {
    const index = this.activeProfileIndex();
    return index !== null ? this.profiles()[index] : null;
  }

  getActiveProfileWidgetStates(): Record<string, Signal<WidgetState>> | null {
    const index = this.activeProfileIndex();
    return index !== null ? this.profileWidgetStates()[index] : null;
  }

  updateWidgetState(widgetName: string, update: Partial<WidgetState>, profileIndex?: number) {
    const index = profileIndex !== undefined ? profileIndex : this.activeProfileIndex();
    if (index === null) return;

    const profileWidgetStates = this.profileWidgetStates()[index];
    if (profileWidgetStates) {
      const widgetState = profileWidgetStates[widgetName];
      if (widgetState) {
        widgetState.update(state => ({ ...state, ...update }));
      }
    }
  }

  getVisibleWidgets = computed(() => {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return [];
    
    return WIDGET_CONFIG.filter(widget => widget.visible).map(widget => ({
      ...widget,
      state: activeProfileWidgetStates[widget.name]
    }));
  });

  setActiveWidget(widgetName: string) {
    this.activeWidgetName.set(widgetName);
    this.updateWidgetState(widgetName, { active: true, visited: true, status: 'in-progress' });
  }

  resetWidgetStates() {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (activeProfileWidgetStates) {
      Object.keys(activeProfileWidgetStates).forEach(widgetName => {
        const widget = WIDGET_CONFIG.find(w => w.name === widgetName);
        if (widget) {
          activeProfileWidgetStates[widgetName].set({ ...widget.state() });
        }
      });
    }
  }

  loadInitialWidget() {
    const firstWidget = this.getVisibleWidgets()[0];
    if (firstWidget) {
      this.setActiveWidget(firstWidget.name);
    }
  }

  async createOrEditProfile(partyId?: string) {
    let profileData: any = {};
    const metadataData = await this.fetchMetadata();

    if (partyId) {
      profileData = await this.fetchPartyData(partyId);
    }

    const profileFormGroup = this.createProfileFormGroup(profileData, metadataData);
    this.profiles.update(profiles => [...profiles, profileFormGroup]);
    this.setActiveProfile(this.profiles().length - 1);
  }

  private createProfileFormGroup(profileData: any = {}, metadataData: any = {}): FormGroup {
    const profileGroup = this.fb.group({});

    WIDGET_CONFIG.forEach(widget => {
      if (widget.visible) {
        profileGroup.addControl(widget.name, this.fb.group({}));
      }
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

  validateAllWidgets(): boolean {
    const activeProfile = this.getActiveProfile();
    if (!activeProfile) return false;

    let isValid = true;
    this.getVisibleWidgets().forEach(widget => {
      const widgetForm = activeProfile.get(widget.name);
      const widgetValid = widgetForm?.valid ?? false;
      this.updateWidgetState(widget.name, { 
        hasError: !widgetValid, 
        status: widgetValid ? 'completed' : 'error' 
      });
      isValid = isValid && widgetValid;
    });
    return isValid;
  }

  acknowledgeWidget(widgetName: string, profileIndex?: number) {
    this.updateWidgetState(widgetName, { acknowledged: true, status: 'completed' }, profileIndex);
  }

  isProfileComplete(): boolean {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return false;

    return this.getVisibleWidgets().every(widget => 
      activeProfileWidgetStates[widget.name]().status === 'completed'
    );
  }

  getCompletionPercentage(): number {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return 0;

    const totalWidgets = this.getVisibleWidgets().length;
    const completedWidgets = this.getVisibleWidgets().filter(widget => 
      activeProfileWidgetStates[widget.name]().status === 'completed'
    ).length;
    return (completedWidgets / totalWidgets) * 100;
  }

  getNextIncompleteWidget(): string | null {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return null;

    const incompleteWidget = this.getVisibleWidgets().find(widget => 
      activeProfileWidgetStates[widget.name]().status !== 'completed'
    );
    return incompleteWidget ? incompleteWidget.name : null;
  }

  // Helper methods
  getProfileCount(): number {
    return this.profiles().length;
  }

  hasActiveProfile(): boolean {
    return this.activeProfileIndex() !== null;
  }

  getProfileWidgetState(profileIndex: number, widgetName: string): WidgetState | null {
    const profileStates = this.profileWidgetStates()[profileIndex];
    return profileStates ? profileStates[widgetName]() : null;
  }

  isWidgetLoaded(widgetName: string): boolean {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    return activeProfileWidgetStates ? activeProfileWidgetStates[widgetName]().visited : false;
  }

  loadAllWidgets() {
    this.getVisibleWidgets().forEach(widget => {
      this.updateWidgetState(widget.name, { visited: true, status: 'in-progress' });
    });
  }

  getWidgetErrors(widgetName: string): any {
    const activeProfile = this.getActiveProfile();
    if (!activeProfile) return null;

    const widgetForm = activeProfile.get(widgetName);
    return widgetForm ? widgetForm.errors : null;
  }

  resetProfile(profileIndex: number) {
    const profile = this.profiles()[profileIndex];
    if (profile) {
      profile.reset();
      this.resetWidgetStates();
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
    <div class="profile-selector">
      <button *ngFor="let profile of profileService.profiles(); let i = index"
              (click)="profileService.setActiveProfile(i)"
              [class.active]="i === profileService.activeProfileIndex()">
        Profile {{ i + 1 }}
      </button>
      <button (click)="createNewProfile()">Add New Profile</button>
    </div>
    <div class="party-builder" *ngIf="profileService.hasActiveProfile()">
      <app-left-menu></app-left-menu>
      <div class="widget-container">
        <ng-container *ngFor="let widget of profileService.getVisibleWidgets()">
          <ng-container #widgetContainer></ng-container>
        </ng-container>
      </div>
    </div>
    <button (click)="editExistingProfile('some-party-id')">Edit Existing Profile</button>
    <button (click)="validateAll()">Validate All</button>
    <button (click)="loadNextIncompleteWidget()">Next Incomplete Widget</button>
    <button (click)="loadAllWidgets()">Load All Widgets</button>
    <button (click)="acknowledgeWidgetForProfile('personal', 0)">Acknowledge Personal Info for Profile 1</button>
  `
})
export class PartyBuilderComponent {
  @ViewChild('widgetContainer', { read: ViewContainerRef, static: false, emitDistinctChangesOnly: false })
  widgetContainers!: ViewContainerRef[];

  profileService = inject(ProfileBuilderService);
  private injector = inject(Injector);

  constructor() {
    effect(() => {
      const activeWidget = this.profileService.getVisibleWidgets().find(w => w.state().active);
      if (activeWidget) {
        this.loadComponent(activeWidget.name);
      }
    }, { injector: this.injector });
  }

  async loadComponent(widgetName: string) {
    const widgetConfig = this.profileService.getVisibleWidgets().find(w => w.name === widgetName);
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
      }
    }
  }

  createNewProfile() {
    this.profileService.addNewProfile();
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

  loadAllWidgets() {
    this.profileService.loadAllWidgets();
    this.profileService.getVisibleWidgets().forEach(widget => {
      this.loadComponent(widget.name);
    });
  }

  acknowledgeWidgetForProfile(widgetName: string, profileIndex: number) {
    this.profileService.acknowledgeWidget(widgetName, profileIndex);
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
  profileService = inject(


---------
import { Injectable, signal, computed, inject, WritableSignal } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { HttpClient } from '@angular/common/http';
import { WIDGET_CONFIG, WidgetConfig, WidgetState } from './widget-config';

@Injectable({ providedIn: 'root' })
export class ProfileBuilderService {
  private fb = inject(FormBuilder);
  private http = inject(HttpClient);

  private profiles: WritableSignal<FormGroup[]> = signal([]);
  private activeProfileIndex: WritableSignal<number | null> = signal(null);
  private profileWidgetStates: WritableSignal<Array<Record<string, WritableSignal<WidgetState>>>> = signal([]);
  private activeWidgetName: WritableSignal<string | null> = signal(null);
  private isLoading: WritableSignal<boolean> = signal(false);

  addNewProfile() {
    const newProfileForm = this.createProfileFormGroup();
    this.profiles.update(profiles => [...profiles, newProfileForm]);
    
    const newProfileWidgetStates = Object.fromEntries(
      WIDGET_CONFIG.map(widget => [
        widget.name, 
        signal<WidgetState>({ ...widget.state() })
      ])
    );
    this.profileWidgetStates.update(states => [...states, newProfileWidgetStates]);
    
    this.setActiveProfile(this.profiles().length - 1);
  }

  setActiveProfile(index: number) {
    if (index >= 0 && index < this.profiles().length) {
      this.activeProfileIndex.set(index);
      this.resetWidgetStates();
      this.loadInitialWidget();
    } else {
      console.error('Invalid profile index');
    }
  }

  getActiveProfile(): FormGroup | null {
    const index = this.activeProfileIndex();
    return index !== null ? this.profiles()[index] : null;
  }

  getActiveProfileWidgetStates(): Record<string, WritableSignal<WidgetState>> | null {
    const index = this.activeProfileIndex();
    return index !== null ? this.profileWidgetStates()[index] : null;
  }

  updateWidgetState(widgetName: string, update: Partial<WidgetState>, profileIndex?: number) {
    const index = profileIndex !== undefined ? profileIndex : this.activeProfileIndex();
    if (index === null) return;

    const profileWidgetStates = this.profileWidgetStates()[index];
    if (profileWidgetStates) {
      const widgetState = profileWidgetStates[widgetName];
      if (widgetState) {
        widgetState.update(state => ({ ...state, ...update }));
      }
    }
  }

  getVisibleWidgets = computed(() => {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return [];
    
    return WIDGET_CONFIG.filter(widget => widget.visible).map(widget => ({
      ...widget,
      state: activeProfileWidgetStates[widget.name]
    }));
  });

  setActiveWidget(widgetName: string) {
    this.activeWidgetName.set(widgetName);
    this.updateWidgetState(widgetName, { active: true, visited: true, status: 'in-progress' });
  }

  resetWidgetStates() {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (activeProfileWidgetStates) {
      Object.keys(activeProfileWidgetStates).forEach(widgetName => {
        const widget = WIDGET_CONFIG.find(w => w.name === widgetName);
        if (widget) {
          activeProfileWidgetStates[widgetName].set({ ...widget.state() });
        }
      });
    }
  }

  async createOrEditProfile(partyId?: string) {
    this.isLoading.set(true);
    try {
      let profileData: any = {};
      const metadataData = await this.fetchMetadata();

      if (partyId) {
        profileData = await this.fetchPartyData(partyId);
      }

      const profileFormGroup = this.createProfileFormGroup(profileData, metadataData);
      this.profiles.update(profiles => [...profiles, profileFormGroup]);
      this.setActiveProfile(this.profiles().length - 1);
    } finally {
      this.isLoading.set(false);
    }
  }

  // ... other methods remain the same ...

  getIsLoading() {
    return this.isLoading.asReadonly();
  }
}
