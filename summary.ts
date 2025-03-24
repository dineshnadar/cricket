// Fixing the cdkDropList connection issue

// 1. Update the getConnectedDropListIds method to ensure it only returns existing IDs

getConnectedDropListIds(): string[] {
  // Start with an array containing just the main container ID
  const ids = ['main-form-container'];
  
  // Use ViewChildren or a different approach to ensure all drop lists are properly registered
  // before attempting to connect them
  
  // Only add IDs for sections that are already in the DOM
  this.formData.forEach(section => {
    if (section.instanceId) {
      ids.push('drop-' + section.instanceId);
      
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

// 2. Update the template to ensure drop lists have consistent IDs
/*
In your template, make sure all cdkDropList elements have proper IDs:

<div id="section-catalog" cdkDropList [cdkDropListData]="catalogData.sections" 
     cdkDropListId="section-catalog"
     [cdkDropListConnectedTo]="getConnectedDropListIds()">
  <!-- ... -->
</div>

<div id="field-catalog" cdkDropList [cdkDropListData]="catalogData.fields"
     cdkDropListId="field-catalog"
     [cdkDropListConnectedTo]="getConnectedDropListIds()">
  <!-- ... -->
</div>

<div class="form-container" cdkDropList [cdkDropListData]="formData" 
     cdkDropListId="main-form-container"
     (cdkDropListDropped)="dropIntoForm($event)">
  <!-- ... -->
</div>
*/

// 3. Add a method to reinitialize connections after DOM changes

// When adding new sections/fields, we need to refresh the connections
reinitializeDropConnections(): void {
  // Using setTimeout to ensure the DOM has updated
  setTimeout(() => {
    // Force Angular to detect changes and update the connections
    this.getConnectedDropListIds();
    
    // If you're using ChangeDetectorRef, you can also call:
    // this.cdr.detectChanges();
  }, 0);
}

// 4. Call reinitializeDropConnections whenever you add a new section or field
// For example, in dropIntoForm, dropIntoSection, and dropIntoSubsection methods:

dropIntoForm(event: CdkDragDrop<any[]>): void {
  // Existing implementation...
  
  if (event.previousContainer !== event.container) {
    // Dropping a catalog item into the form
    const dragData = event.item.data;
    
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
        
        this.formData.push(sectionData);
        
        // Reinitialize drop connections after adding a new section
        this.reinitializeDropConnections();
      }
    }
  }
}

// Similarly, update dropIntoSection and dropIntoSubsection

// 5. Add logging to help diagnose connection issues
ngAfterViewInit() {
  // Log the available IDs for drop lists
  console.log('Connected drop list IDs:', this.getConnectedDropListIds());
  
  // Re-initialize connections after view init
  this.reinitializeDropConnections();
}
