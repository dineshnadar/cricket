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
  fields: FormField[];
  subheaders?: SubheaderData[];
  type: 'form';
}

export interface FormField {
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
      const contentWidth = pageWidth - leftMargin - rightMargin;
      
      let yPos = options.includeHeader ? 80 : 40;
      let pageNum = 1;
      
      if (options.includeHeader) {
        this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                           new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                           options.primaryColor || '#3366cc', options.companyLogo, options.headerText);
      }
      
      for (const item of content.contentItems) {
        if (item.type === 'form') {
          const form = item;
          
          if (yPos + 20 + (form.fields.length * 20) > pageHeight - 60) {
            if (options.includeFooter) {
              this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666', options.footerOptions);
            }
            pdf.addPage();
            pageNum++;
            yPos = options.includeHeader ? 80 : 40;
            
            if (options.includeHeader) {
              this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                 new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                 options.primaryColor || '#3366cc', options.companyLogo, options.headerText);
            }
          }
          
          if (form.title) {
            pdf.setFont('helvetica', 'bold');
            pdf.setFontSize(12);
            pdf.setTextColor(options.primaryColor || '#3366cc');
            pdf.text(form.title, leftMargin, yPos);
            yPos += 20;
          }
          
          pdf.setFont('helvetica', 'normal');
          pdf.setFontSize(10);
          pdf.setTextColor(0, 0, 0);
          
          let fieldCounter = 0;
          for (const field of form.fields) {
            if (yPos + 20 > pageHeight - 60) {
              if (options.includeFooter) {
                this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666', options.footerOptions);
              }
              pdf.addPage();
              pageNum++;
              yPos = options.includeHeader ? 80 : 40;
              
              if (options.includeHeader) {
                this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                   new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                   options.primaryColor || '#3366cc', options.companyLogo, options.headerText);
              }
            }
            
            pdf.setFont('helvetica', 'bold');
            pdf.text(`${field.label}:`, leftMargin, yPos);
            
            pdf.setFont('helvetica', 'normal');
            pdf.text(field.value, pageWidth - rightMargin, yPos, { align: 'right' });
            yPos += 20;
            
            if (field.addDivider) {
              pdf.setDrawColor(220, 220, 220);
              pdf.setLineWidth(0.5);
              pdf.line(leftMargin, yPos - 10, pageWidth - rightMargin, yPos - 10);
            }
            
            if (form.subheaders) {
              const subheader = form.subheaders.find(sh => sh.afterFieldIndex === fieldCounter);
              if (subheader) {
                yPos += 10;
                pdf.setFont('helvetica', 'bold');
                pdf.setFontSize(11);
                pdf.setTextColor(options.secondaryColor || '#666666');
                pdf.text(subheader.text, leftMargin, yPos);
                yPos += 15;
                
                pdf.setTextColor(0, 0, 0);
              }
            }
            
            fieldCounter++;
          }
          
          yPos += 20;
          
        } else if (item.type === 'table') {
          const table = item;
          
          if (table.title) {
            if (yPos + 30 > pageHeight - 60) {
              if (options.includeFooter) {
                this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666', options.footerOptions);
              }
              pdf.addPage();
              pageNum++;
              yPos = options.includeHeader ? 80 : 40;
              
              if (options.includeHeader) {
                this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                   new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                   options.primaryColor || '#3366cc', options.companyLogo, options.headerText);
              }
            }
            
            pdf.setFont('helvetica', 'bold');
            pdf.setFontSize(12);
            pdf.setTextColor(options.primaryColor || '#3366cc');
            pdf.text(table.title, leftMargin, yPos);
            yPos += 20;
          }
          
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
            margin: { left: leftMargin, right: rightMargin },
            didDrawPage: (data) => {
              if (data.pageNumber > pageNum) {
                if (options.includeHeader) {
                  this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                     new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                     options.primaryColor || '#3366cc', options.companyLogo, options.headerText);
                }
                
                if (options.includeFooter) {
                  this.addFooterToPDF(pdf, pageWidth, pageHeight, data.pageNumber, options.secondaryColor || '#666666', options.footerOptions);
                }
                
                pageNum = data.pageNumber;
              }
            }
          });
          
          yPos = (pdf as any).lastAutoTable.finalY + 20;
          
          if (yPos > pageHeight - 80) {
            if (options.includeFooter) {
              this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666', options.footerOptions);
            }
            pdf.addPage();
            pageNum++;
            yPos = options.includeHeader ? 80 : 40;
            
            if (options.includeHeader) {
              this.addHeaderToPDF(pdf, pageWidth, options.customerName || 'Customer', 
                                 new Date().toLocaleDateString(), options.headerTitle || 'COMPANY NAME',
                                 options.primaryColor || '#3366cc', options.companyLogo, options.headerText);
            }
          }
        }
      }
      
      if (options.includeFooter) {
        this.addFooterToPDF(pdf, pageWidth, pageHeight, pageNum, options.secondaryColor || '#666666', options.footerOptions);
      }
      
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
