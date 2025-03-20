// pdf.service.ts
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
  companyLogo?: string; // Base64 encoded image
  primaryColor?: string; // Hex color for headings and highlights
  secondaryColor?: string; // Hex color for secondary elements
}

export interface TableData {
  headers: string[];
  rows: any[][];
  title?: string;
}

export interface FormData {
  title?: string;
  fields: { label: string; value: string }[];
}

export interface PDFContent {
  forms: FormData[];
  tables: TableData[];
  summary?: { label: string; value: string }[];
  notes?: string;
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

  /**
   * Generate PDF from structured data
   */
  generateDataPDF(content: PDFContent, options?: PDFGenerationOptions): Observable<void> {
    const mergedOptions = { ...this.defaultOptions, ...options };
    
    return from(this.generateDataPDFAsync(content, mergedOptions))
      .pipe(
        finalize(() => console.log('PDF generation completed'))
      );
  }

  /**
   * Implementation of PDF generation from data
   */
  private async generateDataPDFAsync(content: PDFContent, options: PDFGenerationOptions): Promise<void> {
    try {
      // Initialize PDF document
      const pdf = new jsPDF({
        orientation: options.orientation,
        unit: 'pt',
        format: options.pageSize
      });
      
      const pageWidth = pdf.internal.pageSize.getWidth();
      const pageHeight = pdf.internal.pageSize.getHeight();
      
      // Set initial position
      let yPos = options.includeHeader ? 80 : 40;
      let pageNum = 1;
      
      // Add header to first page
      if (options.includeHeader) {
        this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                            new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                            options.primaryColor || '#3366cc', options.companyLogo);
      }
      
      // Process form sections
      for (const form of content.forms) {
        // Check if we need a page break
        if (yPos + 20 + (form.fields.length * 20) > pageHeight - 60) {
          if (options.includeFooter) {
            this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
          }
          pdf.addPage();
          pageNum++;
          yPos = options.includeHeader ? 80 : 40;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                options.primaryColor || '#3366cc', options.companyLogo);
          }
        }
        
        // Add form title if provided
        if (form.title) {
          pdf.setFont('helvetica', 'bold');
          pdf.setFontSize(12);
          pdf.setTextColor(options.primaryColor || '#3366cc');
          pdf.text(form.title, 40, yPos);
          yPos += 20;
        }
        
        // Add form fields
        pdf.setFont('helvetica', 'normal');
        pdf.setFontSize(10);
        pdf.setTextColor(0, 0, 0);
        
        for (const field of form.fields) {
          // Check if we need a page break
          if (yPos + 20 > pageHeight - 60) {
            if (options.includeFooter) {
              this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
            }
            pdf.addPage();
            pageNum++;
            yPos = options.includeHeader ? 80 : 40;
            
            if (options.includeHeader) {
              this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                  new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                  options.primaryColor || '#3366cc', options.companyLogo);
            }
          }
          
          pdf.setFont('helvetica', 'bold');
          pdf.text(`${field.label}:`, 40, yPos);
          pdf.setFont('helvetica', 'normal');
          pdf.text(field.value, 200, yPos);
          yPos += 20;
        }
        
        // Add spacing after form
        yPos += 20;
      }
      
      // Process table sections
      for (const table of content.tables) {
        // Add table title if provided
        if (table.title) {
          // Check if we need a page break
          if (yPos + 30 > pageHeight - 60) {
            if (options.includeFooter) {
              this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
            }
            pdf.addPage();
            pageNum++;
            yPos = options.includeHeader ? 80 : 40;
            
            if (options.includeHeader) {
              this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                  new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                  options.primaryColor || '#3366cc', options.companyLogo);
            }
          }
          
          pdf.setFont('helvetica', 'bold');
          pdf.setFontSize(12);
          pdf.setTextColor(options.primaryColor || '#3366cc');
          pdf.text(table.title, 40, yPos);
          yPos += 20;
        }
        
        // Add table using autotable
        autoTable(pdf, {
          startY: yPos,
          head: [table.headers],
          body: table.rows,
          theme: 'grid',
          headStyles: {
            fillColor: options.primaryColor || '#3366cc',
            textColor: 255,
            fontStyle: 'bold'
          },
          margin: { left: 40, right: 40 },
          didDrawPage: (data) => {
            // Add header and footer on new pages created by the table
            if (data.pageNumber > pageNum) {
              if (options.includeHeader) {
                this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                    new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                    options.primaryColor || '#3366cc', options.companyLogo);
              }
              
              if (options.includeFooter) {
                this.addFooterToPDF(pdf, pageWidth, pageHeight, data.pageNumber, options.secondaryColor || '#666666');
              }
              
              pageNum = data.pageNumber;
            }
          }
        });
        
        // Update position after table
        yPos = (pdf as any).lastAutoTable.finalY + 20;
        
        // Check if we're near the bottom of the page
        if (yPos > pageHeight - 80) {
          if (options.includeFooter) {
            this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
          }
          pdf.addPage();
          pageNum++;
          yPos = options.includeHeader ? 80 : 40;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                options.primaryColor || '#3366cc', options.companyLogo);
          }
        }
      }
      
      // Add summary if provided
      if (content.summary && content.summary.length > 0) {
        // Check if we need a page break
        if (yPos + 20 + (content.summary.length * 20) > pageHeight - 60) {
          if (options.includeFooter) {
            this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
          }
          pdf.addPage();
          pageNum++;
          yPos = options.includeHeader ? 80 : 40;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                options.primaryColor || '#3366cc', options.companyLogo);
          }
        }
        
        // Add summary title
        pdf.setFont('helvetica', 'bold');
        pdf.setFontSize(12);
        pdf.setTextColor(options.primaryColor || '#3366cc');
        pdf.text('Summary', 40, yPos);
        yPos += 20;
        
        // Add summary items
        pdf.setFontSize(10);
        pdf.setTextColor(0, 0, 0);
        
        for (const item of content.summary) {
          // Check if we need a page break
          if (yPos + 20 > pageHeight - 60) {
            if (options.includeFooter) {
              this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
            }
            pdf.addPage();
            pageNum++;
            yPos = options.includeHeader ? 80 : 40;
            
            if (options.includeHeader) {
              this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                  new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                  options.primaryColor || '#3366cc', options.companyLogo);
            }
          }
          
          pdf.setFont('helvetica', 'bold');
          pdf.text(`${item.label}:`, 40, yPos);
          pdf.setFont('helvetica', 'normal');
          pdf.text(item.value, 200, yPos);
          yPos += 20;
        }
        
        yPos += 20;
      }
      
      // Add notes if provided
      if (content.notes) {
        // Check if we need a page break
        if (yPos + 60 > pageHeight - 60) {
          if (options.includeFooter) {
            this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
          }
          pdf.addPage();
          pageNum++;
          yPos = options.includeHeader ? 80 : 40;
          
          if (options.includeHeader) {
            this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                options.primaryColor || '#3366cc', options.companyLogo);
          }
        }
        
        // Add notes title
        pdf.setFont('helvetica', 'bold');
        pdf.setFontSize(12);
        pdf.setTextColor(options.primaryColor || '#3366cc');
        pdf.text('Notes', 40, yPos);
        yPos += 20;
        
        // Add notes content with word wrapping
        pdf.setFont('helvetica', 'normal');
        pdf.setFontSize(10);
        pdf.setTextColor(0, 0, 0);
        
        const textLines = pdf.splitTextToSize(content.notes, pageWidth - 80);
        
        for (const line of textLines) {
          // Check if we need a page break
          if (yPos + 20 > pageHeight - 60) {
            if (options.includeFooter) {
              this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
            }
            pdf.addPage();
            pageNum++;
            yPos = options.includeHeader ? 80 : 40;
            
            if (options.includeHeader) {
              this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                  new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                  options.primaryColor || '#3366cc', options.companyLogo);
            }
          }
          
          pdf.text(line, 40, yPos);
          yPos += 15;
        }
      }
      
      // Add footer to last page
      if (options.includeFooter) {
        this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666');
      }
      
      // Save the PDF
      pdf.save(options.filename);
    } catch (error) {
      console.error('Error generating PDF:', error);
      throw error;
    }
  }
  
  private addHeaderToPDF(
    pdf: jsPDF,
    pageWidth: number,
    customerName: string,
    date: string,
    companyName: string,
    primaryColor: string,
    logo?: string
  ): void {
    // Add logo if provided
    if (logo) {
      pdf.addImage(logo, 'JPEG', 40, 20, 60, 30);
      
      // Adjust company name position if logo is present
      pdf.setFont('helvetica', 'bold');
      pdf.setFontSize(16);
      pdf.setTextColor(primaryColor);
      pdf.text(companyName, 120, 35);
    } else {
      pdf.setFont('helvetica', 'bold');
      pdf.setFontSize(16);
      pdf.setTextColor(primaryColor);
      pdf.text(companyName, 40, 35);
    }
    
    // Add customer info
    pdf.setTextColor(90, 90, 90);
    pdf.setFontSize(10);
    pdf.setFont('helvetica', 'normal');
    pdf.text(`Customer: ${customerName}`, pageWidth - 40, 25, { align: 'right' });
    pdf.text(`Date: ${date}`, pageWidth - 40, 40, { align: 'right' });
    
    // Add separator line
    pdf.setDrawColor(200, 200, 200);
    pdf.setLineWidth(0.5);
    pdf.line(40, 50, pageWidth - 40, 50);
  }
  
  private addFooterToPDF(
    pdf: jsPDF,
    pageWidth: number,
    pageHeight: number,
    pageNum: number,
    secondaryColor: string
  ): void {
    // Add separator line
    pdf.setDrawColor(200, 200, 200);
    pdf.setLineWidth(0.5);
    pdf.line(40, pageHeight - 50, pageWidth - 40, pageHeight - 50);
    
    // Add footer text
    pdf.setFont('helvetica', 'normal');
    pdf.setFontSize(9);
    pdf.setTextColor(secondaryColor);
    pdf.text('Generated with Angular PDF Service', 40, pageHeight - 30);
    
    // Add page number
    pdf.text(`Page ${pageNum}`, pageWidth - 40, pageHeight - 30, { align: 'right' });
  }
}
