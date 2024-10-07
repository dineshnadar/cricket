// table.types.ts
export interface TableColumn {
  key: string;
  header: string;
  visible: boolean;
  clickable?: boolean;
  rowClickable?: boolean;
  cellTemplate?: any;
  useComputedValue?: boolean;
}

export interface FieldItem {
  label: string;
  fldName: string;
  value: any;
  computedValue?: any;
  type: string;
  editable: boolean;
  visible: boolean;
  isValid: boolean;
  errors?: any;
  readOnly?: boolean;
  fields?: { [key: string]: FieldItem };
}

export interface TableConfig {
  enableSorting?: boolean;
  defaultSortColumn?: string;
  defaultSortDirection?: 'asc' | 'desc';
}

--------xxx----

import { Component, computed, signal, inject, OnInit, ChangeDetectionStrategy } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { TableComponent } from './table.component';
import { FormExtensionService } from './form-extension.service';
import { TableColumn, FieldItem, TableConfig } from './table.types';
import { ApiService } from './api.service';
import { memoize } from './memoize.util';

interface ColumnConfig {
  key: string;
  header: string;
  visible: boolean;
  useComputedValue?: boolean;
  conditionalKey?: (row: any) => string;
  nestedKey?: string[];
}

interface TableMode {
  isReadOnly: boolean;
  editableColumns?: string[];
}

@Component({
  selector: 'app-complex-form',
  standalone: true,
  imports: [TableComponent],
  template: `
    @if (complexForm(); as form) {
      <form [formGroup]="form">
        <app-table
          [tableForm]="form"
          [columns]="columns()"
          [rows]="memoizedRows()"
          [config]="tableConfig"
          [currentPage]="currentPage()"
          [totalPages]="totalPages()"
          [canAddRow]="canAddRow()"
          [tableMode]="tableMode()"
          (toggleColumnVisibility)="toggleColumnVisibility($event)"
          (toggleEdit)="toggleEdit($event)"
          (editRow)="editRow($event)"
          (deleteRow)="deleteRow($event)"
          (pageChange)="onPageChange($event)"
          (addRow)="addRow()"
          (cellClick)="onCellClick($event)"
          (rowClick)="onRowClick($event)"
          (sortChange)="onSortChange($event)">
        </app-table>
      </form>
    } @else {
      <app-skeleton-loader></app-skeleton-loader>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class ComplexFormComponent implements OnInit {
  private fb = inject(FormBuilder);
  private formExtensionService = inject(FormExtensionService);
  private apiService = inject(ApiService);

  complexForm = signal<FormGroup | null>(null);
  
  columns = signal<ColumnConfig[]>([
    { key: 'name', header: 'Name', visible: true },
    { key: 'age', header: 'Age', visible: true, useComputedValue: true },
    { 
      key: 'email', 
      header: 'Email', 
      visible: true, 
      conditionalKey: (row) => row.usaStatus === 'Y' ? 'emailInfo.emailType' : 'emailInfo.email',
      nestedKey: ['emailInfo', 'email']
    },
    { key: 'usaStatus', header: 'USA Status', visible: true }
  ]);

  private rawRows = signal<FieldItem[]>([]);
  
  memoizedRows = memoize(() => this.generateRows(this.rawRows(), this.columns()));

  tableConfig: TableConfig = {
    enablePagination: true,
    enableSorting: true,
    defaultSortColumn: 'name',
    defaultSortDirection: 'asc'
  };

  tableMode = signal<TableMode>({ isReadOnly: false });

  currentPage = signal(1);
  totalPages = computed(() => Math.ceil(this.rawRows().length / this.pageSize));
  canAddRow = computed(() => {
    const rows = this.rawRows();
    return !this.tableMode().isReadOnly && rows.length > 0 && rows.every(row => row.isValid);
  });

  private pageSize = 10;

  ngOnInit() {
    this.loadFormData();
  }

  private async loadFormData() {
    try {
      const data = await this.apiService.getFormData().toPromise();
      this.initForm(data);
    } catch (error) {
      console.error('Error loading form data:', error);
      // Handle error (e.g., show error message to user)
    }
  }

  private initForm(data: any): void {
    const form = this.fb.group({
      rows: this.fb.array([])
    });
    
    this.formExtensionService.extendControl(form, {
      label: 'Table Data',
      fldName: 'tableData',
      type: 'group'
    });

    this.formExtensionService.extendControl(form.get('rows')!, {
      label: 'Rows',
      fldName: 'rows',
      type: 'array'
    });

    if (data && data.rows) {
      data.rows.forEach((rowData: any) => {
        const row = this.createRow(rowData);
        (form.get('rows') as FormArray).push(row);
      });
    }

    this.complexForm.set(form);
    this.updateRawRows();
  }

  private updateRawRows() {
    const form = this.complexForm();
    if (!form) return;
    const uiView = this.formExtensionService.getUIReadView(form);
    this.rawRows.set(uiView.find(item => item.fldName === 'rows')?.fields || []);
  }

  private generateRows(rawRows: FieldItem[], columns: ColumnConfig[]): FieldItem[] {
    return rawRows.map(row => {
      const newRow: FieldItem = { ...row, fields: {} };
      columns.forEach(column => {
        let value;
        if (column.conditionalKey) {
          const key = column.conditionalKey(row);
          value = this.getNestedValue(row, key.split('.'));
        } else if (column.nestedKey) {
          value = this.getNestedValue(row, column.nestedKey);
        } else {
          value = row.fields![column.key];
        }
        newRow.fields![column.key] = value;
      });
      return newRow;
    });
  }

  private getNestedValue(obj: any, keys: string[]): any {
    return keys.reduce((acc, key) => (acc && acc[key] !== undefined) ? acc[key] : undefined, obj);
  }

  get rowsArray(): FormArray | null {
    return this.complexForm()?.get('rows') as FormArray | null;
  }

  toggleColumnVisibility(columnKey: string): void {
    this.columns.update(cols => 
      cols.map(col => col.key === columnKey ? { ...col, visible: !col.visible } : col)
    );
  }

  toggleEdit(event: {rowName: string, key: string}): void {
    if (this.tableMode().isReadOnly || 
        (this.tableMode().editableColumns && !this.tableMode().editableColumns.includes(event.key))) {
      return;
    }

    const rowsArray = this.rowsArray;
    if (!rowsArray) return;

    const rowIndex = this.rawRows().findIndex(row => row.fldName === event.rowName);
    if (rowIndex === -1) return;

    const row = rowsArray.at(rowIndex) as FormGroup;
    if (row.get('editable')?.value && !row.get('readOnly')?.value) {
      const field = row.get(event.key) as FormGroup;
      if (field) {
        field.patchValue({ editing: !field.get('editing')?.value });
        this.updateRawRows();
      }
    }
  }

  editRow(rowName: string): void {
    if (this.tableMode().isReadOnly) return;
    console.log('Editing row', rowName);
    // Implement edit logic if needed
  }

  deleteRow(rowName: string): void {
    if (this.tableMode().isReadOnly) return;
    const rowsArray = this.rowsArray;
    if (!rowsArray) return;

    const rowIndex = this.rawRows().findIndex(row => row.fldName === rowName);
    if (rowIndex !== -1) {
      rowsArray.removeAt(rowIndex);
      this.updateRawRows();
    }
  }

  onPageChange(page: number): void {
    if (this.tableConfig.enablePagination) {
      this.currentPage.set(page);
    }
  }

  addRow(): void {
    if (this.tableMode().isReadOnly) return;
    const rowsArray = this.rowsArray;
    if (rowsArray && this.canAddRow()) {
      const newRow = this.createRow();
      rowsArray.push(newRow);
      this.updateRawRows();
    }
  }

  onCellClick(event: {rowName: string, key: string}): void {
    console.log(`Cell clicked: Row ${event.rowName}, Column ${event.key}`);
    // Implement your click logic here
  }

  onRowClick(rowName: string): void {
    console.log(`Row clicked: ${rowName}`);
    // Implement your row click logic here
  }

  onSortChange(event: {column: string, direction: 'asc' | 'desc'}): void {
    console.log(`Sort changed: Column ${event.column}, Direction ${event.direction}`);
    // Implement your sorting logic here if needed
  }

  setTableMode(mode: TableMode): void {
    this.tableMode.set(mode);
  }

  private createRow(data?: any): FormGroup {
    const row = this.fb.group({
      name: this.fb.group({
        value: [data?.name || '', Validators.required],
        editing: [false]
      }),
      age: this.fb.group({
        value: [data?.age || '', [Validators.required, Validators.min(0)]],
        editing: [false]
      }),
      emailInfo: this.fb.group({
        email: [data?.emailInfo?.email || '', [Validators.required, Validators.email]],
        emailType: [data?.emailInfo?.emailType || ''],
        editing: [false]
      }),
      usaStatus: [data?.usaStatus || 'N'],
      editable: [true],
      readOnly: [false]
    });

    this.formExtensionService.extendControl(row, {
      label: `Row ${this.rowsArray?.length ?? 0 + 1}`,
      fldName: `row${this.rowsArray?.length ?? 0 + 1}`,
      type: 'group'
    });

    ['name', 'age', 'emailInfo', 'usaStatus'].forEach(key => {
      this.formExtensionService.extendControl(row.get(key)!, {
        label: key.charAt(0).toUpperCase() + key.slice(1),
        fldName: key,
        type: key === 'emailInfo' ? 'group' : 'field'
      });
    });

    this.formExtensionService.registerCustomComputation(row.get('age')!, memoize((control) => {
      const age = control.get('value')?.value;
      return age ? `${age} years old` : '';
    }));

    return row;
  }
}

-------x--------x-
  this.setTableMode({ isReadOnly: true }); // For full read-only mode
this.setTableMode({ isReadOnly: false, editableColumns: ['name', 'age'] }); // For partial edit mode


---------------x-----------x

{
  key: 'email',
  header: 'Email',
  visible: true,
  conditionalKey: (row) => row.usaStatus === 'Y' ? 'emailInfo.emailType' : 'emailInfo.email',
  nestedKey: ['emailInfo', 'email']
}

-------x-----
  // table.component.ts
import { Component, Input, Output, EventEmitter, ChangeDetectionStrategy, computed, Signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormGroup, ReactiveFormsModule, FormArray } from '@angular/forms';
import { TableColumn, FieldItem, TableConfig, TableMode } from './table.types';
import { memoize } from './memoize.util';

@Component({
  selector: 'app-table',
  standalone: true,
  imports: [CommonModule, ReactiveFormsModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
  template: `
    <table>
      <thead>
        <tr>
          @for (column of visibleColumns(); track column.key) {
            <th (click)="config.enableSorting ? sort(column.key) : null">
              {{ column.header }}
              <button (click)="toggleColumnVisibility.emit(column.key); $event.stopPropagation()">
                {{ column.visible ? 'Hide' : 'Show' }}
              </button>
              @if (config.enableSorting && sortColumn() === column.key) {
                <span>{{ sortDirection() === 'asc' ? '▲' : '▼' }}</span>
              }
            </th>
          }
          @if (!tableMode.isReadOnly) {
            <th>Actions</th>
          }
        </tr>
      </thead>
      <tbody>
        @for (row of displayedRows(); track row.fldName) {
          <tr [class.clickable]="isRowClickable()"
              (click)="onRowClick(row.fldName)">
            @for (column of visibleColumns(); track column.key) {
              <td>
                @if (column.cellTemplate) {
                  <ng-container *ngTemplateOutlet="column.cellTemplate; context: { $implicit: row.fields![column.key], row: row, column: column }"></ng-container>
                } @else {
                  <ng-container *ngTemplateOutlet="defaultCellTemplate; context: { $implicit: row.fields![column.key], row: row, column: column }"></ng-container>
                }
              </td>
            }
            @if (!tableMode.isReadOnly) {
              <td>
                @if (row.editable && !row.readOnly) {
                  <button (click)="editRow.emit(row.fldName)">Edit</button>
                }
                @if (!row.readOnly) {
                  <button (click)="deleteRow.emit(row.fldName)">Delete</button>
                }
              </td>
            }
          </tr>
        }
      </tbody>
    </table>
    @if (config.enablePagination) {
      <div class="pagination">
        <button (click)="pageChange.emit(currentPage - 1)" [disabled]="currentPage === 1">Previous</button>
        <span>Page {{ currentPage }} of {{ totalPages }}</span>
        <button (click)="pageChange.emit(currentPage + 1)" [disabled]="currentPage === totalPages">Next</button>
      </div>
    }
    @if (!tableMode.isReadOnly) {
      <button (click)="addRow.emit()" [disabled]="!canAddRow">Add Row</button>
    }

    <ng-template #defaultCellTemplate let-field let-row="row" let-column="column">
      @if (isEditing(row.fldName, column.key) && isEditable(column.key) && row.editable && !row.readOnly) {
        <input [formControl]="getFormControl(row.fldName, column.key)"
               (blur)="toggleEdit.emit({rowName: row.fldName, key: column.key})"
               (click)="$event.stopPropagation()">
      } @else {
        <span 
          (click)="onCellClick(row.fldName, column.key, $event)"
          [class.clickable]="column.clickable"
        >
          {{ column.useComputedValue ? field.computedValue : field.value }}
        </span>
      }
    </ng-template>
  `,
  styles: [`
    .clickable { cursor: pointer; }
  `]
})
export class TableComponent {
  @Input({ required: true }) tableForm!: FormGroup;
  @Input() columns!: TableColumn[];
  @Input({ required: true }) rows!: FieldItem[];
  @Input() config: TableConfig = {};
  @Input() currentPage!: number;
  @Input() totalPages!: number;
  @Input({ required: true }) canAddRow!: boolean;
  @Input() tableMode: TableMode = { isReadOnly: false };
  @Output() toggleColumnVisibility = new EventEmitter<string>();
  @Output() toggleEdit = new EventEmitter<{rowName: string, key: string}>();
  @Output() editRow = new EventEmitter<string>();
  @Output() deleteRow = new EventEmitter<string>();
  @Output() pageChange = new EventEmitter<number>();
  @Output() addRow = new EventEmitter<void>();
  @Output() cellClick = new EventEmitter<{rowName: string, key: string}>();
  @Output() rowClick = new EventEmitter<string>();
  @Output() sortChange = new EventEmitter<{column: string, direction: 'asc' | 'desc'}>();

  private sortColumnSignal = signal<string | null>(null);
  private sortDirectionSignal = signal<'asc' | 'desc'>('asc');

  sortColumn = this.sortColumnSignal.asReadonly();
  sortDirection = this.sortDirectionSignal.asReadonly();

  visibleColumns = computed(() => this.columns.filter(col => col.visible));

  private memoizedSortFunction = memoize((a: FieldItem, b: FieldItem, column: string, direction: 'asc' | 'desc') => {
    const aValue = this.getSortValue(a, column);
    const bValue = this.getSortValue(b, column);
    return direction === 'asc' ? this.compare(aValue, bValue) : this.compare(bValue, aValue);
  });

  displayedRows = computed(() => {
    let rows = this.rows;
    if (this.config.enableSorting && this.sortColumn()) {
      rows = [...rows].sort((a, b) => 
        this.memoizedSortFunction(a, b, this.sortColumn()!, this.sortDirection())
      );
    }
    if (this.config.enablePagination) {
      const pageSize = 10; // You can make this configurable
      const startIndex = (this.currentPage - 1) * pageSize;
      rows = rows.slice(startIndex, startIndex + pageSize);
    }
    return rows;
  });

  rowsFormArray = computed(() => this.tableForm.get('rows') as FormArray);

  constructor() {
    if (this.config.enableSorting && this.config.defaultSortColumn) {
      this.sortColumnSignal.set(this.config.defaultSortColumn);
      this.sortDirectionSignal.set(this.config.defaultSortDirection || 'asc');
    }
  }

  isEditing = memoize((rowName: string, key: string): boolean => {
    const rowIndex = this.rows.findIndex(row => row.fldName === rowName);
    return this.rowsFormArray().at(rowIndex).get(key)?.get('editing')?.value ?? false;
  });

  isEditable(columnKey: string): boolean {
    if (this.tableMode.isReadOnly) return false;
    if (!this.tableMode.editableColumns) return true;
    return this.tableMode.editableColumns.includes(columnKey);
  }

  isRowClickable = (): boolean => {
    return this.columns.some(col => col.rowClickable);
  }

  getFormControl = memoize((rowName: string, key: string) => {
    const rowIndex = this.rows.findIndex(row => row.fldName === rowName);
    return this.rowsFormArray().at(rowIndex).get(key);
  });

  onCellClick(rowName: string, key: string, event: Event): void {
    event.stopPropagation();
    const column = this.columns.find(col => col.key === key);
    if (column?.clickable) {
      this.cellClick.emit({rowName, key});
    } else if (this.isEditable(key)) {
      const row = this.rows.find(r => r.fldName === rowName);
      if (row?.editable && !row.readOnly) {
        this.toggleEdit.emit({rowName, key});
      }
    }
  }

  onRowClick(rowName: string): void {
    if (this.isRowClickable()) {
      this.rowClick.emit(rowName);
    }
  }

  sort(column: string): void {
    if (this.sortColumn() === column) {
      this.sortDirectionSignal.update(dir => dir === 'asc' ? 'desc' : 'asc');
    } else {
      this.sortColumnSignal.set(column);
      this.sortDirectionSignal.set('asc');
    }
    this.sortChange.emit({column: this.sortColumn()!, direction: this.sortDirection()});
  }

  private getSortValue(row: FieldItem, key: string): any {
    const column = this.columns.find(col => col.key === key);
    const field = row.fields![key];
    return column?.useComputedValue ? field.computedValue : field.value;
  }

  private compare(a: any, b: any): number {
    if (a === b) return 0;
    return a < b ? -1 : 1;
  }
}
