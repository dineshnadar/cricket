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
  private profileWidgetStates: WritableSignal<Record<string, WidgetState>[]> = signal([]);
  private activeWidgetName: WritableSignal<string | null> = signal(null);
  private isLoading: WritableSignal<boolean> = signal(false);

  private initialWidgetState: WidgetState = {
    hasError: false,
    visited: false,
    acknowledged: false,
    active: false,
    status: 'pending'
  };

  addNewProfile() {
    const newProfileForm = this.createProfileFormGroup();
    this.profiles.update(profiles => [...profiles, newProfileForm]);
    
    const newProfileWidgetStates = WIDGET_CONFIG.reduce((acc, widget) => {
      acc[widget.name] = { ...this.initialWidgetState };
      return acc;
    }, {} as Record<string, WidgetState>);

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

  getActiveProfileWidgetStates(): Record<string, WidgetState> | null {
    const index = this.activeProfileIndex();
    return index !== null ? this.profileWidgetStates()[index] : null;
  }

  updateWidgetState(widgetName: string, update: Partial<WidgetState>, profileIndex?: number) {
    const index = profileIndex !== undefined ? profileIndex : this.activeProfileIndex();
    if (index === null) return;

    this.profileWidgetStates.update(allStates => {
      const newAllStates = [...allStates];
      newAllStates[index] = {
        ...newAllStates[index],
        [widgetName]: { ...newAllStates[index][widgetName], ...update }
      };
      return newAllStates;
    });
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
    const index = this.activeProfileIndex();
    if (index !== null) {
      this.profileWidgetStates.update(allStates => {
        const newAllStates = [...allStates];
        newAllStates[index] = WIDGET_CONFIG.reduce((acc, widget) => {
          acc[widget.name] = { ...this.initialWidgetState };
          return acc;
        }, {} as Record<string, WidgetState>);
        return newAllStates;
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
    this.isLoading.set(true);
    try {
      let profileData: any = {};
      const metadataData = await this.fetchMetadata();

      if (partyId) {
        // Editing existing profile
        profileData = await this.fetchPartyData(partyId);
        const existingProfileIndex = this.findProfileIndexByPartyId(partyId);
        
        if (existingProfileIndex !== -1) {
          // Update existing profile
          this.updateProfile(existingProfileIndex, profileData, metadataData);
          this.setActiveProfile(existingProfileIndex);
        } else {
          console.error('Profile not found for editing');
        }
      } else {
        // Creating new profile
        this.addNewProfile();
        const newProfileIndex = this.profiles().length - 1;
        this.updateProfile(newProfileIndex, profileData, metadataData);
      }
    } finally {
      this.isLoading.set(false);
    }
  }

  private updateProfile(index: number, profileData: any, metadataData: any) {
    const profileForm = this.profiles()[index];
    if (profileForm) {
      // Update form with new data
      profileForm.patchValue(profileData);
      
      // Update widget states based on the new data
      WIDGET_CONFIG.forEach(widget => {
        if (profileData[widget.name]) {
          this.updateWidgetState(widget.name, { visited: true }, index);
        }
      });
    }
  }

  private findProfileIndexByPartyId(partyId: string): number {
    return this.profiles().findIndex(profile => profile.get('partyId')?.value === partyId);
  }

  private createProfileFormGroup(profileData: any = {}, metadataData: any = {}): FormGroup {
    const profileGroup = this.fb.group({
      partyId: [''], // Ensure partyId is part of the form
      // Add other common fields here
    });

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
    const index = profileIndex !== undefined ? profileIndex : this.activeProfileIndex();
    if (index === null) {
      console.error('No profile specified or active');
      return;
    }

    this.updateWidgetState(widgetName, { acknowledged: true, status: 'completed' }, index);
  }

  isProfileComplete(): boolean {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return false;

    return Object.values(activeProfileWidgetStates).every(state => state.status === 'completed');
  }

  getCompletionPercentage(): number {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return 0;

    const totalWidgets = Object.keys(activeProfileWidgetStates).length;
    const completedWidgets = Object.values(activeProfileWidgetStates).filter(state => state.status === 'completed').length;
    return (completedWidgets / totalWidgets) * 100;
  }

  getNextIncompleteWidget(): string | null {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return null;

    const incompleteWidget = Object.entries(activeProfileWidgetStates).find(([_, state]) => state.status !== 'completed');
    return incompleteWidget ? incompleteWidget[0] : null;
  }

  getProfileCount(): number {
    return this.profiles().length;
  }

  hasActiveProfile(): boolean {
    return this.activeProfileIndex() !== null;
  }

  getProfileWidgetState(profileIndex: number, widgetName: string): WidgetState | null {
    const profileStates = this.profileWidgetStates()[profileIndex];
    return profileStates ? profileStates[widgetName] : null;
  }

  isWidgetLoaded(widgetName: string): boolean {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    return activeProfileWidgetStates ? activeProfileWidgetStates[widgetName].visited : false;
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

  getIsLoading() {
    return this.isLoading;
  }
}

--------------------

@Component({
  // ... component metadata
})
export class PartyBuilderComponent {
  // ... other properties and methods

  validateAll() {
    const isValid = this.profileService.validateAllWidgets();
    console.log(isValid ? 'All widgets are valid' : 'There are validation errors');
    console.log('Completion percentage:', this.profileService.getCompletionPercentage());
  }

  loadAllWidgets() {
    this.profileService.loadAllWidgets();
    // You might want to trigger loading of all widget components here
    this.profileService.getVisibleWidgets().forEach(widget => {
      this.loadComponent(widget.name);
    });
  }

  loadNextIncompleteWidget() {
    const nextWidgetName = this.profileService.getNextIncompleteWidget();
    if (nextWidgetName) {
      this.profileService.setActiveWidget(nextgetNextIncompleteWidgetName);
      this.loadComponent(nextWidgetName);
    } else {
      console.log('All widgets are complete!');
    }
  }

  // ... rest of the component
}

-------

@Injectable({ providedIn: 'root' })
export class ProfileBuilderService {
  // ... other properties and methods remain the same

  validateAllWidgets(): boolean {
    const activeProfile = this.getActiveProfile();
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfile || !activeProfileWidgetStates) return false;

    let isValid = true;
    Object.entries(activeProfileWidgetStates).forEach(([widgetName, state]) => {
      if (WIDGET_CONFIG.find(w => w.name === widgetName)?.visible) {
        const widgetForm = activeProfile.get(widgetName);
        const widgetValid = widgetForm?.valid ?? false;
        this.updateWidgetState(widgetName, { 
          hasError: !widgetValid, 
          status: widgetValid ? 'completed' : 'error' 
        });
        isValid = isValid && widgetValid;
      }
    });
    return isValid;
  }

  loadAllWidgets() {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return;

    Object.keys(activeProfileWidgetStates).forEach(widgetName => {
      if (WIDGET_CONFIG.find(w => w.name === widgetName)?.visible) {
        this.updateWidgetState(widgetName, { visited: true, status: 'in-progress' });
      }
    });
  }

  getNextIncompleteWidget(): string | null {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return null;

    const incompleteWidget = Object.entries(activeProfileWidgetStates).find(([widgetName, state]) => 
      WIDGET_CONFIG.find(w => w.name === widgetName)?.visible && state.status !== 'completed'
    );
    return incompleteWidget ? incompleteWidget[0] : null;
  }

  // Additional helper method to get completion percentage
  getCompletionPercentage(): number {
    const activeProfileWidgetStates = this.getActiveProfileWidgetStates();
    if (!activeProfileWidgetStates) return 0;

    const visibleWidgets = WIDGET_CONFIG.filter(w => w.visible);
    const completedWidgets = visibleWidgets.filter(w => 
      activeProfileWidgetStates[w.name]?.status === 'completed'
    );
    return (completedWidgets.length / visibleWidgets.length) * 100;
  }

  // ... rest of the service remains the same
}
