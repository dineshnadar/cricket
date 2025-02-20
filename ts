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
  let tooltipService: jest.Mocked<TooltipService>;

  beforeEach(async () => {
    // Create mock TooltipService with proper typing
    const mockTooltipService = {
      show: jest.fn(),
      hide: jest.fn(),
      // Use a getter for isVisible to make it mockable
      get isVisible() {
        return jest.fn().mockReturnValue(false);
      }
    };

    await TestBed.configureTestingModule({
      declarations: [TestHostComponent],
      imports: [TooltipDirective],
      providers: [
        { 
          provide: TooltipService, 
          useValue: mockTooltipService 
        }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(TestHostComponent);
    component = fixture.componentInstance;
    buttonDebugEl = fixture.debugElement.query(By.directive(TooltipDirective));
    tooltipService = TestBed.inject(TooltipService) as jest.Mocked<TooltipService>;
    
    fixture.detectChanges();
  });

  it('should create an instance', () => {
    const directive = buttonDebugEl.injector.get(TooltipDirective);
    expect(directive).toBeTruthy();
  });

  describe('Tooltip Interactions', () => {
    it('should show tooltip on mouseenter', () => {
      // Ensure isVisible returns false
      (tooltipService.isVisible as jest.Mock).mockReturnValue(false);

      // Simulate mouseenter event
      buttonDebugEl.triggerEventHandler('mouseenter', null);

      // Expect show method to be called
      expect(tooltipService.show).toHaveBeenCalledTimes(1);
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

  // ... rest of the test suite remains the same
});
