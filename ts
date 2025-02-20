// tooltip.directive.spec.ts
import { Component, DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { TooltipDirective } from './tooltip.directive';
import { TooltipService } from './tooltip.service';

// Test host component
@Component({
  template: `
    <button 
      [appTooltip]="{
        message: 'Test Tooltip', 
        position: 'top'
      }"
    >
      Hover Me
    </button>
  `
})
class TestHostComponent {}

describe('TooltipDirective', () => {
  let component: TestHostComponent;
  let fixture: ComponentFixture<TestHostComponent>;
  let buttonDebugEl: DebugElement;
  let tooltipService: TooltipService;

  beforeEach(async () => {
    // Create mock TooltipService
    const mockTooltipService = {
      show: jest.fn(),
      hide: jest.fn(),
      isVisible: jest.fn().mockReturnValue(false)
    };

    await TestBed.configureTestingModule({
      declarations: [TestHostComponent],
      imports: [TooltipDirective],
      providers: [
        { provide: TooltipService, useValue: mockTooltipService }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(TestHostComponent);
    component = fixture.componentInstance;
    buttonDebugEl = fixture.debugElement.query(By.directive(TooltipDirective));
    tooltipService = TestBed.inject(TooltipService);
    
    fixture.detectChanges();
  });

  it('should create an instance', () => {
    const directive = buttonDebugEl.injector.get(TooltipDirective);
    expect(directive).toBeTruthy();
  });

  describe('Tooltip Interactions', () => {
    it('should show tooltip on mouseenter', () => {
      // Simulate mouseenter event
      buttonDebugEl.triggerEventHandler('mouseenter', null);

      // Expect show method to be called
      expect(tooltipService.show).toHaveBeenCalledTimes(1);
    });

    it('should hide tooltip on mouseleave', () => {
      // Simulate mouseenter and then mouseleave
      buttonDebugEl.triggerEventHandler('mouseenter', null);
      buttonDebugEl.triggerEventHandler('mouseleave', null);

      // Expect hide method to be called
      expect(tooltipService.hide).toHaveBeenCalledTimes(1);
    });

    it('should not show tooltip if already visible', () => {
      // Mock tooltip as already visible
      (tooltipService.isVisible as jest.Mock).mockReturnValue(true);

      // Simulate mouseenter
      buttonDebugEl.triggerEventHandler('mouseenter', null);

      // Expect show method not to be called
      expect(tooltipService.show).not.toHaveBeenCalled();
    });
  });

  describe('Configuration', () => {
    it('should pass correct configuration to tooltip service', () => {
      // Simulate mouseenter
      buttonDebugEl.triggerEventHandler('mouseenter', null);

      // Expect show method to be called with correct arguments
      expect(tooltipService.show).toHaveBeenCalledWith(
        expect.objectContaining({
          message: 'Test Tooltip',
          position: 'top'
        }),
        expect.anything()
      );
    });
  });

  describe('Lifecycle', () => {
    it('should hide tooltip on component destroy', () => {
      const directive = buttonDebugEl.injector.get(TooltipDirective);
      
      // Simulate mouseenter to make tooltip visible
      buttonDebugEl.triggerEventHandler('mouseenter', null);

      // Call ngOnDestroy manually
      directive.ngOnDestroy();

      // Expect hide method to be called
      expect(tooltipService.hide).toHaveBeenCalledTimes(1);
    });
  });
});
