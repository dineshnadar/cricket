import { TestBed } from '@angular/core/testing';
import { DomSanitizer } from '@angular/platform-browser';
import { SecurityContext } from '@angular/core';
import { TooltipService } from './tooltip.service';
import { ElementRef, Renderer2, RendererFactory2 } from '@angular/core';

// Explicitly type mocks
type MockDomSanitizer = {
  sanitize: jest.Mock;
  bypassSecurityTrustHtml: jest.Mock;
};

type MockRenderer = {
  createElement: jest.Mock;
  addClass: jest.Mock;
  appendChild: jest.Mock;
  removeChild: jest.Mock;
  setProperty: jest.Mock;
};

type MockRendererFactory = {
  createRenderer: jest.Mock;
};

describe('TooltipService', () => {
  let service: TooltipService;
  let mockDomSanitizer: MockDomSanitizer;
  let mockRenderer: MockRenderer;
  let mockRendererFactory: MockRendererFactory;
  let mockElementRef: ElementRef;

  beforeEach(() => {
    // Create mock dependencies with explicit typing
    mockDomSanitizer = {
      sanitize: jest.fn(),
      bypassSecurityTrustHtml: jest.fn()
    };

    mockRenderer = {
      createElement: jest.fn(),
      addClass: jest.fn(),
      appendChild: jest.fn(),
      removeChild: jest.fn(),
      setProperty: jest.fn(),
    };

    mockRendererFactory = {
      createRenderer: jest.fn().mockReturnValue(mockRenderer)
    };

    mockElementRef = {
      nativeElement: document.createElement('div')
    };

    // Configure TestBed
    TestBed.configureTestingModule({
      providers: [
        TooltipService,
        { provide: DomSanitizer, useValue: mockDomSanitizer },
        { provide: RendererFactory2, useValue: mockRendererFactory }
      ]
    });

    // Inject service
    service = TestBed.inject(TooltipService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  describe('Sanitization Methods', () => {
    it('should sanitize string content', () => {
      const htmlContent = '<strong>Test</strong>';
      mockDomSanitizer.sanitize.mockReturnValue('sanitized-content');

      // Use type assertion to access private method
      const result = (service as any)['sanitizeMessage'](htmlContent);

      expect(mockDomSanitizer.sanitize).toHaveBeenCalledWith(
        SecurityContext.HTML, 
        htmlContent
      );
      expect(result).toBe('sanitized-content');
    });

    it('should handle SafeHtml content', () => {
      const safeHtml = {} as any;
      mockDomSanitizer.sanitize.mockReturnValue('safe-content');

      // Use type assertion to access private method
      const result = (service as any)['sanitizeMessage'](safeHtml);

      expect(mockDomSanitizer.sanitize).toHaveBeenCalledWith(
        SecurityContext.HTML, 
        safeHtml
      );
      expect(result).toBe('safe-content');
    });
  });

  describe('Tooltip Visibility', () => {
    const testConfig = {
      message: 'Test Tooltip',
      position: 'top'
    };

    it('should initially have isVisible as false', () => {
      expect(service.isVisible()).toBe(false);
    });

    it('should show tooltip', () => {
      // Mock sanitization
      mockDomSanitizer.sanitize.mockReturnValue('sanitized-message');

      // Show tooltip
      service.show(testConfig, mockElementRef);

      // Assertions
      expect(service.isVisible()).toBe(true);
      expect(service.content()).toBe('sanitized-message');
      expect(service.position()).toBe('top');
      
      // Verify renderer methods
      expect(mockRenderer.createElement).toHaveBeenCalledWith('div');
      expect(mockRenderer.addClass).toHaveBeenCalledWith(expect.anything(), 'tooltip');
      expect(mockRenderer.appendChild).toHaveBeenCalled();
    });

    it('should hide tooltip', () => {
      // Show tooltip first
      service.show(testConfig, mockElementRef);
      expect(service.isVisible()).toBe(true);

      // Hide tooltip
      service.hide();

      // Assertions
      expect(service.isVisible()).toBe(false);
      expect(service.content()).toBe('');
      expect(m
