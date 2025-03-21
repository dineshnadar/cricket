import { Injectable } from '@angular/core';
import { jsPDF } from 'jspdf';
import autoTable from 'jspdf-autotable';
import { Observable, from } from 'rxjs';
import { finalize } from 'rxjs/operators';

export interface PDFGenerationOptions {
  headerTitle?: string;
  customerName?: string;
  includeFooter?: boolean;
  includeHeader?: boolean;
  filename?: string;
  pageSize?: string;
  orientation?: 'p' | 'portrait' | 'l' | 'landscape';
  companyLogo?: string;
  primaryColor?: string;
  secondaryColor?: string;
  headerText?: string;
  footerOptions?: FooterOptions;
}

export interface FooterOptions {
  leftText?: string;
  leftSecondaryText?: string;
  rightText?: string;
  showTimestamp?: boolean;
  timestampPrefix?: string;
  showPageNumber?: boolean;
  topText?: string;
}

export interface TableData {
  headers: string[];
  rows: any[][];
  title?: string;
  type: 'table';
}

export interface FormData {
  title?: string;
  subtitle?: string;
  fields: FormField[];
  subheaders?: SubheaderData[];
  type: 'form';
}

export interface FormField {
  type?: 'field' | 'subheader';
  label: string; 
  value: string;
  addDivider?: boolean;
}

export interface SubheaderData {
  text: string;
  afterFieldIndex: number;
}

export type ContentItem = FormData | TableData;

export interface PDFContent {
  contentItems: ContentItem[];
}

@Injectable({
  providedIn: 'root'
})
export class PdfService {
  private defaultOptions: PDFGenerationOptions = {
    headerTitle: 'COMPANY NAME',
    customerName: 'Customer',
    includeFooter: true,
    includeHeader: true,
    filename: 'document.pdf',
    pageSize: 'a4',
    orientation: 'portrait',
    primaryColor: '#3366cc',
    secondaryColor: '#666666'
  };

  constructor() {}

  generateDataPDF(content: PDFContent, options?: PDFGenerationOptions): Observable<void> {
    const mergedOptions = { ...this.defaultOptions, ...options };
    
    return from(this.generateDataPDFAsync(content, mergedOptions))
      .pipe(
        finalize(() => console.log('PDF generation completed'))
      );
  }

  private async generateDataPDFAsync(content: PDFContent, options: PDFGenerationOptions): Promise<void> {
    try {
      const pdf = new jsPDF({
        orientation: options.orientation,
        unit: 'pt',
        format: options.pageSize
      });
      
      const pageWidth = pdf.internal.pageSize.getWidth();
      const pageHeight = pdf.internal.pageSize.getHeight();
      
      const leftMargin = 40;
      const rightMargin = 40;
      
      // Calculate header and footer heights to prevent overlap
      const headerHeight = options.includeHeader ? 60 : 20;
      const footerHeight = options.includeFooter ? 60 : 20;
      
      let yPos = options.includeHeader ? 80 : 40;
      let pageNum = 1;
      
      if (options.includeHeader) {
        this.addHeaderToPDF(
          pdf, 
          pageWidth, 
          options.customerName || 'Customer', 
          new Date().toLocaleDateString(), 
          options.headerTitle || 'COMPANY NAME',
          options.primaryColor || '#3366cc', 
          options.companyLogo, 
          options.headerText
        );
      }
      
      for (const item of content.contentItems) {
        if (item.type === 'form') {
          yPos = this.renderFormAsTable(
            pdf, 
            item, 
            yPos, 
            options, 
            leftMargin, 
            rightMargin,
            pageWidth,
            pageHeight,
            headerHeight,
            footerHeight
          );
        } else if (item.type === 'table') {
          yPos = this.renderTableWithPagination(
            pdf,
            item,
            yPos,
            options,
            leftMargin,
            rightMargin,
            pageWidth,
            pageHeight,
            headerHeight,
            footerHeight
          );
        }
        
        // Add spacing between content items
        yPos += 20;
        
        // Check if we need a page break before the next content item
        if (yPos > pageHeight - footerHeight - 40) {
          if (options.includeFooter) {
            this.addFooterToPDF(
              pdf, 
              pageWidth, 
              pageHeight, 
              pageNum, 
              options.secondaryColor || '#666666', 
              options.footerOptions
            );
          }
          
          pdf.addPage();
          pageNum++;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(
              pdf, 
              pageWidth, 
              options.customerName || 'Customer', 
              new Date().toLocaleDateString(), 
              options.headerTitle || 'COMPANY NAME',
              options.primaryColor || '#3366cc', 
              options.companyLogo, 
              options.headerText
            );
          }
          
          yPos = headerHeight;
        }
      }
      
      if (options.includeFooter) {
        this.addFooterToPDF(
          pdf, 
          pageWidth, 
          pageHeight, 
          pageNum, 
          options.secondaryColor || '#666666', 
          options.footerOptions
        );
      }
      
      pdf.save(options.filename);
    } catch (error) {
      console.error('Error generating PDF:', error);
      throw error;
    }
  }
  
  private renderFormAsTable(
    pdf: jsPDF,
    form: FormData,
    yPos: number,
    options: PDFGenerationOptions,
    leftMargin: number,
    rightMargin: number,
    pageWidth: number,
    pageHeight: number,
    headerHeight: number,
    footerHeight: number
  ): number {
    // Get current page number
    let pageNum = 1;
    try {
      pageNum = pdf.internal.pages.length - 1 || 1;
    } catch (e) {
      pageNum = 1;
    }
    
    // Calculate available width for the table
    const availableWidth = pageWidth - leftMargin - rightMargin;
    
    // Calculate column widths in pixels
    const labelWidth = availableWidth * 0.4;
    const valueWidth = availableWidth * 0.6;
    
    // Add form title if provided
    if (form.title) {
      // Check if we have enough space for title
      if (yPos + 35 > pageHeight - footerHeight) {
        if (options.includeFooter) {
          this.addFooterToPDF(
            pdf, 
            pageWidth, 
            pageHeight, 
            pageNum, 
            options.secondaryColor || '#666666', 
            options.footerOptions
          );
        }
        
        pdf.addPage();
        pageNum++;
        
        if (options.includeHeader) {
          this.addHeaderToPDF(
            pdf, 
            pageWidth, 
            options.customerName || 'Customer', 
            new Date().toLocaleDateString(), 
            options.headerTitle || 'COMPANY NAME',
            options.primaryColor || '#3366cc', 
            options.companyLogo, 
            options.headerText
          );
        }
        
        yPos = headerHeight;
      }
      
      pdf.setFont('helvetica', 'bold');
      pdf.setFontSize(12);
      pdf.setTextColor(options.primaryColor || '#3366cc');
      pdf.text(form.title, leftMargin, yPos);
      yPos += 20;
      
      // Add subtitle if provided
      if (form.subtitle) {
        pdf.setFont('helvetica', 'italic');
        pdf.setFontSize(10);
        pdf.setTextColor(options.secondaryColor || '#666666');
        pdf.text(form.subtitle, leftMargin, yPos);
        yPos += 15;
      }
    }
    
    // Convert form fields to table rows
    const tableRows: any[][] = [];
    
    for (let i = 0; i < form.fields.length; i++) {
      const field = form.fields[i];
      
      // Check if this is a subheader field
      if (field.type === 'subheader') {
        // Add a subheader row (spans both columns)
        tableRows.push([{
          content: field.label,
          colSpan: 2,
          styles: { 
            fontStyle: 'bold', 
            textColor: options.secondaryColor || [100, 100, 100],
            cellPadding: { top: 5, bottom: 2 }
          }
        }]);
      } else {
        // Create the styles for both cells
        const labelStyles: any = { 
          fontStyle: 'bold', 
          halign: 'left'
        };
        
        const valueStyles: any = { 
          halign: 'right'
        };
        
        // If this field needs a divider, add bottom border to both cells
        if (field.addDivider) {
          // Instead of using lineWidths, set direct border properties
          labelStyles.cellPadding = { top: 5, right: 5, bottom: 10, left: 5 };
          valueStyles.cellPadding = { top: 5, right: 5, bottom: 10, left: 5 };
          
          // Mark cells to have divider drawn later
          labelStyles.drawDivider = true;
          valueStyles.drawDivider = true;
        }
        
        // Add regular field as a row
        tableRows.push([
          { 
            content: `${field.label}:`, 
            styles: labelStyles
          },
          { 
            content: field.value, 
            styles: valueStyles
          }
        ]);
      }
      
      // Check for subheaders defined in subheaders array
      if (form.subheaders) {
        const subheader = form.subheaders.find(sh => sh.afterFieldIndex === i);
        if (subheader) {
          tableRows.push([{
            content: subheader.text,
            colSpan: 2,
            styles: { 
              fontStyle: 'bold', 
              textColor: options.secondaryColor || [100, 100, 100],
              cellPadding: { top: 10, bottom: 5 }
            }
          }]);
        }
      }
    }
    
    // Use autotable to render the form
    autoTable(pdf, {
      startY: yPos,
      body: tableRows,
      theme: 'plain',
      styles: {
        fontSize: 10,
        cellPadding: 5,
        lineWidth: 0 // No borders by default
      },
      columnStyles: {
        0: { cellWidth: labelWidth },
        1: { cellWidth: valueWidth }
      },
      margin: { 
        left: leftMargin, 
        right: rightMargin,
        top: headerHeight, 
        bottom: footerHeight 
      },
      showHead: false,
      willDrawCell: (data) => {
        // Check if content would overlap with footer
        if (data.cursor.y + data.row.height > pageHeight - footerHeight - 10) {
          // We need to move to next page
          if (options.includeFooter) {
            this.addFooterToPDF(
              pdf, 
              pageWidth, 
              pageHeight, 
              pageNum, 
              options.secondaryColor || '#666666', 
              options.footerOptions
            );
          }
          
          pdf.addPage();
          pageNum++;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(
              pdf, 
              pageWidth, 
              options.customerName || 'Customer', 
              new Date().toLocaleDateString(), 
              options.headerTitle || 'COMPANY NAME',
              options.primaryColor || '#3366cc', 
              options.companyLogo, 
              options.headerText
            );
          }
          
          // Reset cursor position
          data.cursor.y = headerHeight + 10;
        }
      },
      didDrawCell: (data) => {
        // For cells with dividers, manually draw the divider
        if (data.cell.raw && data.cell.raw.styles && data.cell.raw.styles.drawDivider) {
          // Draw a divider line under the cell
          const x1 = data.cell.x;
          const x2 = data.cell.x + data.cell.width;
          const y = data.cell.y + data.cell.height;
          
          pdf.setDrawColor(220, 220, 220);
          pdf.setLineWidth(0.5);
          pdf.line(x1, y, x2, y);
        }
      },
      didDrawPage: (data) => {
        // If a new page was created
        if (data.pageNumber > pageNum) {
          pageNum = data.pageNumber;
          
          // Add header to new page
          if (options.includeHeader) {
            this.addHeaderToPDF(
              pdf, 
              pageWidth, 
              options.customerName || 'Customer', 
              new Date().toLocaleDateString(), 
              options.headerTitle || 'COMPANY NAME',
              options.primaryColor || '#3366cc', 
              options.companyLogo, 
              options.headerText
            );
          }
          
          // Add footer to new page
          if (options.includeFooter) {
            this.addFooterToPDF(
              pdf, 
              pageWidth, 
              pageHeight, 
              pageNum, 
              options.secondaryColor || '#666666', 
              options.footerOptions
            );
          }
          
          // Reset cursor position to avoid header overlap
          data.cursor.y = headerHeight;
        }
      }
    });
    
    // Return new Y position
    return (pdf as any).lastAutoTable.finalY + 10;
  }
  
  private renderTableWithPagination(
    pdf: jsPDF,
    table: TableData,
    yPos: number,
    options: PDFGenerationOptions,
    leftMargin: number,
    rightMargin: number,
    pageWidth: number,
    pageHeight: number,
    headerHeight: number,
    footerHeight: number
  ): number {
    // Get current page number
    let pageNum = 1;
    try {
      pageNum = pdf.internal.pages.length - 1 || 1;
    } catch (e) {
      pageNum = 1;
    }
    
    // Add table title if provided
    if (table.title) {
      // Check if we have enough space for title
      if (yPos + 35 > pageHeight - footerHeight) {
        if (options.includeFooter) {
          this.addFooterToPDF(
            pdf, 
            pageWidth, 
            pageHeight, 
            pageNum, 
            options.secondaryColor || '#666666', 
            options.footerOptions
          );
        }
        
        pdf.addPage();
        pageNum++;
        
        if (options.includeHeader) {
          this.addHeaderToPDF(
            pdf, 
            pageWidth, 
            options.customerName || 'Customer', 
            new Date().toLocaleDateString(), 
            options.headerTitle || 'COMPANY NAME',
            options.primaryColor || '#3366cc', 
            options.companyLogo, 
            options.headerText
          );
        }
        
        yPos = headerHeight;
      }
      
      pdf.setFont('helvetica', 'bold');
      pdf.setFontSize(12);
      pdf.setTextColor(options.primaryColor || '#3366cc');
      pdf.text(table.title, leftMargin, yPos);
      yPos += 20;
    }
    
    // Use autotable to render the table
    autoTable(pdf, {
      startY: yPos,
      head: [table.headers],
      body: table.rows,
      theme: 'grid',
      headStyles: {
        fillColor: options.primaryColor || [51, 102, 204],
        textColor: [255, 255, 255],
        fontStyle: 'bold'
      },
      margin: { 
        left: leftMargin, 
        right: rightMargin,
        top: headerHeight, 
        bottom: footerHeight 
      },
      willDrawCell: (data) => {
        // Check if content would overlap with footer
        if (data.cursor.y + data.row.height > pageHeight - footerHeight - 10) {
          // We need to move to next page
          if (options.includeFooter) {
            this.addFooterToPDF(
              pdf, 
              pageWidth, 
              pageHeight, 
              pageNum, 
              options.secondaryColor || '#666666', 
              options.footerOptions
            );
          }
          
          pdf.addPage();
          pageNum++;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(
              pdf, 
              pageWidth, 
              options.customerName || 'Customer', 
              new Date().toLocaleDateString(), 
              options.headerTitle || 'COMPANY NAME',
              options.primaryColor || '#3366cc', 
              options.companyLogo, 
              options.headerText
            );
          }
          
          // Reset cursor position
          data.cursor.y = headerHeight + 10;
        }
      },
      didDrawPage: (data) => {
        // If a new page was created
        if (data.pageNumber > pageNum) {
          pageNum = data.pageNumber;
          
          // Add header to new page
          if (options.includeHeader) {
            this.addHeaderToPDF(
              pdf, 
              pageWidth, 
              options.customerName || 'Customer', 
              new Date().toLocaleDateString(), 
              options.headerTitle || 'COMPANY NAME',
              options.primaryColor || '#3366cc', 
              options.companyLogo, 
              options.headerText
            );
          }
          
          // Add footer to new page
          if (options.includeFooter) {
            this.addFooterToPDF(
              pdf, 
              pageWidth, 
              pageHeight, 
              pageNum, 
              options.secondaryColor || '#666666', 
              options.footerOptions
            );
          }
          
          // Reset cursor position to avoid header overlap
          data.cursor.y = headerHeight;
        }
      }
    });
    
    // Return new Y position
    return (pdf as any).lastAutoTable.finalY + 10;
  }
  
  private addHeaderToPDF(
    pdf: jsPDF,
    pageWidth: number,
    customerName: string,
    date: string,
    companyName: string,
    primaryColor: string,
    logo?: string,
    headerText?: string
  ): void {
    const leftMargin = 40;
    const rightMargin = 40;
    
    if (headerText) {
      pdf.setFont('helvetica', 'normal');
      pdf.setFontSize(8);
      pdf.setTextColor(100, 100, 100);
      pdf.text(headerText, pageWidth / 2, 15, { align: 'center' });
    }
    
    if (logo) {
      pdf.addImage(logo, 'JPEG', leftMargin, 20, 60, 30);
      
      pdf.setFont('helvetica', 'bold');
      pdf.setFontSize(16);
      pdf.setTextColor(primaryColor);
      pdf.text(companyName, 120, 35);
    } else {
      pdf.setFont('helvetica', 'bold');
      pdf.setFontSize(16);
      pdf.setTextColor(primaryColor);
      pdf.text(companyName, leftMargin, 35);
    }
    
    pdf.setTextColor(90, 90, 90);
    pdf.setFontSize(10);
    pdf.setFont('helvetica', 'normal');
    pdf.text(`Customer: ${customerName}`, pageWidth - rightMargin, 25, { align: 'right' });
    pdf.text(`Date: ${date}`, pageWidth - rightMargin, 40, { align: 'right' });
    
    pdf.setDrawColor(200, 200, 200);
    pdf.setLineWidth(0.5);
    pdf.line(leftMargin, 50, pageWidth - rightMargin, 50);
  }
  
  private addFooterToPDF(
    pdf: jsPDF,
    pageWidth: number,
    pageHeight: number,
    pageNum: number,
    secondaryColor: string,
    footerOptions?: FooterOptions
  ): void {
    const leftMargin = 40;
    const rightMargin = 40;
    const footerY = pageHeight - 30;
    
    const options = footerOptions || {
      leftText: 'Generated with Angular PDF Service',
      leftSecondaryText: 'Confidential Document',
      rightText: 'Authorized Distribution Only',
      showTimestamp: true,
      timestampPrefix: 'Created On:',
      showPageNumber: true
    };
    
    pdf.setDrawColor(200, 200, 200);
    pdf.setLineWidth(0.5);
    pdf.line(leftMargin, pageHeight - 50, pageWidth - rightMargin, pageHeight - 50);
    
    if (options.topText) {
      pdf.setFont('helvetica', 'normal');
      pdf.setFontSize(8);
      pdf.setTextColor(secondaryColor);
      pdf.text(options.topText, pageWidth / 2, pageHeight - 55, { align: 'center' });
    }
    
    pdf.setFont('helvetica', 'normal');
    pdf.setFontSize(9);
    pdf.setTextColor(secondaryColor);
    
    if (options.leftText) {
      pdf.text(options.leftText, leftMargin, footerY);
    }
    
    if (options.leftSecondaryText) {
      pdf.text(options.leftSecondaryText, leftMargin, footerY + 12);
    }
    
    if (options.showPageNumber !== false) {
      const pageText = `Page ${pageNum}`;
      const pageTextWidth = pdf.getTextWidth(pageText);
      pdf.text(pageText, pageWidth / 2 - pageTextWidth / 2, footerY);
    }
    
    if (options.rightText) {
      pdf.text(options.rightText, pageWidth - rightMargin, footerY - 12, { align: 'right' });
    }
    
    if (options.showTimestamp !== false) {
      const now = new Date();
      const timestamp = now.toLocaleDateString() + ' ' + 
                      now.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
      const prefix = options.timestampPrefix ? `${options.timestampPrefix} ` : '';
      pdf.text(`${prefix}${timestamp}`, pageWidth - rightMargin, footerY, { align: 'right' });
    }
  }
}
