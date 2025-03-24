// COMPLETE FIXED VERSION OF CDK DRAG AND DROP IMPLEMENTATION

// First, we need to import the Angular CDK Drag Drop classes correctly
import { Component, OnInit, ViewChild, ElementRef, ChangeDetectorRef, AfterViewInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { CdkDragDrop, moveItemInArray, transferArrayItem, CdkDrag, CdkDropList, DragDropModule } from '@angular/cdk/drag-drop';

// Update these methods in the FormBuilderComponent class:

// 1. Add a ChangeDetectorRef to the constructor
constructor(private cdr: ChangeDetectorRef) { }

// 2. Implement AfterViewInit
ngAfterViewInit() {
  // Log available IDs for debugging
  console.log('Connected drop list IDs:', this.getConnectedDropListIds());
  // Force an initial refresh of drop lists
  this.reinitializeDropConnections();
}

// 3. Updated getConnectedDropListIds method
getConnectedDropListIds(): string[] {
  // Include all static and dynamic drop list IDs
  const ids = ['main-form-container', 'section-catalog', 'field-catalog'];
  
  // Add IDs for all sections in formData
  this.formData.forEach(section => {
    if (section.instanceId) {
      const sectionId = 'drop-' + section.instanceId;
      ids.push(sectionId);
      
      // Add IDs for all subsections
      if (section.subSections && section.subSections.length > 0) {
        section.subSections.forEach((subsection: any) => {
          if (subsection.instanceId) {
            ids.push('drop-' + subsection.instanceId);
          }
        });
      }
    }
  });
  
  return ids;
}

// 4. Add a method to reinitialize connections
reinitializeDropConnections(): void {
  setTimeout(() => {
    // Calculate new connected IDs
    const connectIds = this.getConnectedDropListIds();
    console.log('Refreshing drop connections with IDs:', connectIds);
    
    // Force change detection to update connections
    this.cdr.detectChanges();
  }, 0);
}

// 5. Fixed dropIntoForm method
dropIntoForm(event: CdkDragDrop<any[]>): void {
  console.log('Dropped into form!', event);
  
  if (event.previousContainer === event.container) {
    // Reordering sections within the form
    moveItemInArray(
      event.container.data,
      event.previousIndex,
      event.currentIndex
    );
  } else {
    // Dropping from catalog into the form
    const dragData = event.item.data;
    console.log('Dragged data:', dragData);
    
    if (dragData.type === 'section') {
      const catalogSection = this.catalogData.sections.find(s => s.id === dragData.id);
      if (catalogSection) {
        const instanceId = "instance_" + this.instanceCounter++;
        
        const sectionData = {
          instanceId: instanceId,
          referenceId: dragData.id,
          type: "section",
          name: catalogSection.name,
          label: catalogSection.label || catalogSection.name,
          fields: [],
          subSections: [],
          overrideProperties: {}
        };
        
        // Add the section to formData
        this.formData.push(sectionData);
        
        // Reinitialize drop connections after adding a new section
        this.reinitializeDropConnections();
      }
    }
  }
}

// 6. Fixed dropIntoSection method
dropIntoSection(event: CdkDragDrop<any>, section: any): void {
  console.log('Dropped into section!', event, section);
  
  if (event.previousContainer !== event.container) {
    // Dropping from catalog into a section
    const dragData = event.item.data;
    console.log('Dragged data into section:', dragData);
    
    if (dragData.type === 'section') {
      // Add a subsection
      const catalogSection = this.catalogData.sections.find(s => s.id === dragData.id);
      if (catalogSection) {
        const instanceId = "instance_" + this.instanceCounter++;
        
        const subSectionData = {
          instanceId: instanceId,
          referenceId: dragData.id,
          type: "section",
          name: catalogSection.name,
          label: catalogSection.label || catalogSection.name,
          fields: [],
          subSections: [],
          overrideProperties: {}
        };
        
        if (!section.subSections) {
          section.subSections = [];
        }
        
        section.subSections.push(subSectionData);
        
        // Reinitialize after adding a subsection
        this.reinitializeDropConnections();
      }
    } else if (dragData.type === 'field') {
      // Add a field
      const catalogField = this.catalogData.fields.find(f => f.id === dragData.id);
      if (catalogField) {
        const instanceId = "instance_" + this.instanceCounter++;
        
        const fieldData = {
          instanceId: instanceId,
          referenceId: dragData.id,
          type: catalogField.type,
          name: catalogField.name,
          label: catalogField.label || catalogField.name,
          overrideProperties: {}
        };
        
        if (!section.fields) {
          section.fields = [];
        }
        
        section.fields.push(fieldData);
      }
    }
  }
}

// 7. Fixed dropIntoSubsection method
dropIntoSubsection(event: CdkDragDrop<any>, subsection: any): void {
  console.log('Dropped into subsection!', event, subsection);
  
  if (event.previousContainer !== event.container) {
    // Dropping from catalog into a subsection
    const dragData = event.item.data;
    console.log('Dragged data into subsection:', dragData);
    
    if (dragData.type === 'field') {
      // Add a field to subsection
      const catalogField = this.catalogData.fields.find(f => f.id === dragData.id);
      if (catalogField) {
        const instanceId = "instance_" + this.instanceCounter++;
        
        const fieldData = {
          instanceId: instanceId,
          referenceId: dragData.id,
          type: catalogField.type,
          name: catalogField.name,
          label: catalogField.label || catalogField.name,
          overrideProperties: {}
        };
        
        if (!subsection.fields) {
          subsection.fields = [];
        }
        
        subsection.fields.push(fieldData);
      }
    }
  }
}

// 8. Add data-transfer ability for drag-drop to work between lists
// Update your template to include proper predicates and cdkDropListGroup

/*
<div cdkDropListGroup>
  <!-- Section catalog -->
  <div id="section-catalog" 
       cdkDropList 
       cdkDropListId="section-catalog"
       [cdkDropListData]="catalogData.sections" 
       [cdkDropListConnectedTo]="getConnectedDropListIds()"
       [cdkDropListEnterPredicate]="canEnter"
       class="catalog">
    <div class="section-catalog-item" 
         *ngFor="let section of catalogData.sections" 
         cdkDrag 
         [cdkDragData]="{ id: section.id, type: 'section' }">
      <!-- content -->
    </div>
  </div>
  
  <!-- Field catalog -->
  <div id="field-catalog" 
       cdkDropList 
       cdkDropListId="field-catalog"
       [cdkDropListData]="catalogData.fields"
       [cdkDropListConnectedTo]="getConnectedDropListIds()"
       [cdkDropListEnterPredicate]="canEnter"
       class="catalog">
    <div class="field-catalog-item" 
         *ngFor="let field of catalogData.fields" 
         cdkDrag 
         [cdkDragData]="{ id: field.id, type: 'field' }">
      <!-- content -->
    </div>
  </div>
  
  <!-- Form Container -->
  <div class="form-container" 
       cdkDropList 
       cdkDropListId="main-form-container"
       [cdkDropListData]="formData" 
       [cdkDropListConnectedTo]="getConnectedDropListIds()"
       [cdkDropListEnterPredicate]="canEnterForm"
       (cdkDropListDropped)="dropIntoForm($event)">
    <!-- content -->
  </div>
</div>
*/

// 9. Add predicates to control what can be dropped where
canEnter = (drag: CdkDrag<any>, drop: CdkDropList<any>) => {
  // All catalog items can enter any drop area
  if (drag.dropContainer.id === 'section-catalog' || 
      drag.dropContainer.id === 'field-catalog') {
    return true;
  }
  
  // Default is to allow the drop
  return true;
};

canEnterForm = (drag: CdkDrag<any>, drop: CdkDropList<any>) => {
  // Only sections can be dropped directly into the form container
  if (drag.data?.type === 'section') {
    return true;
  }
  return false;
};

// 10. Ensure the component initializes with the proper connections
ngOnInit(): void {
  // Initialize counters based on the catalog data
  this.updateCounters();
  
  // Initialize the JSON editor with the catalog data
  this.jsonEditor = JSON.stringify(this.catalogData, null, 2);
  
  // Pre-calculate connected IDs
  console.log('Initial connected IDs:', this.getConnectedDropListIds());
}
