import { TestBed } from '@angular/core/testing';
import { DomSanitizer } from '@angular/platform-browser';
import { SecurityContext } from '@angular/core';
import { TooltipService } from './tooltip.service';
import { ElementRef, Renderer2, RendererFactory2 } from '@angular/core';

describe('TooltipService', () => {
  let service: TooltipService;
  let mockDomSanitizer: jest.Mocked<DomSanitizer>;
  let mockRenderer: jest.Mocked<Renderer2>;
  let mockRendererFactory: jest.Mocked<RendererFactory2>;
  let mockElementRef: ElementRef;

  beforeEach(() => {
    // Create mock dependencies
    mockDomSanitizer = {
      sanitize: jest.fn(),
      bypassSecurityTrustHtml: jest.fn()
    } as any;

    mockRenderer = {
      createElement: jest.fn(),
      addClass: jest.fn(),
      appendChild: jest.fn(),
      removeChild: jest.fn(),
      setProperty: jest.fn(),
    } as any;

    mockRendererFactory = {
      createRenderer: jest.fn().mockReturnValue(mockRenderer)
    } as any;

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

      const result = service['sanitizeMessage'](htmlContent);

      expect(mockDomSanitizer.sanitize).toHaveBeenCalledWith(
        SecurityContext.HTML, 
        htmlContent
      );
      expect(result).toBe('sanitized-content');
    });

    it('should handle SafeHtml content', () => {
      const safeHtml = {} as any;
      mockDomSanitizer.sanitize.mockReturnValue('safe-content');

      const result = service['sanitizeMessage'](safeHtml);

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
      expect(mockRenderer.removeChild).toHaveBeenCalled();
    });
  });

  describe('Tooltip Configuration', () => {
    it('should use default position when not provided', () => {
      const config = {
        message: 'Default Tooltip'
      };

      // Mock sanitization
      mockDomSanitizer.sanitize.mockReturnValue('sanitized-message');

      service.show(config, mockElementRef);

      expect(service.position()).toBe('top');
    });

    it('should override default configuration', () => {
      const config = {
        message: 'Custom Tooltip',
        position: 'bottom'
      };

      // Mock sanitization
      mockDomSanitizer.sanitize.mockReturnValue('sanitized-message');

      service.show(config, mockElementRef);

      expect(service.position()).toBe('bottom');
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty message', () => {
      const config = {
        message: ''
      };

      // Mock sanitization
      mockDomSanitizer.sanitize.mockReturnValue('');

      service.show(config, mockElementRef);

      expect(service.content()).toBe('');
    });

    it('should handle multiple show calls', () => {
      const config1 = { message: 'Tooltip 1' };
      const config2 = { message: 'Tooltip 2' };

      // Mock sanitization
      mockDomSanitizer.sanitize.mockReturnValue('sanitized-message');

      // Show first tooltip
      service.show(config1, mockElementRef);
      expect(service.content()).toBe('sanitized-message');

      // Show second tooltip
      service.show(config2, mockElementRef);
      expect(service.content()).toBe('sanitized-message');

      // Hide tooltip
      service.hide();
      expect(service.isVisible()).toBe(false);
    });
  });
});
