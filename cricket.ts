// libs/shared/guards/src/lib/parameter-configs.ts

import { EntitlementConstants as EC } from './entitlement-constants';

export interface ParamConfig {
  context: string;
  required?: string[];
  optional?: string[];
  validate?: { 
    [key: string]: { 
      validate: (value: string) => boolean; 
      errorMessage: string; 
    } | { 
      validate: (value: string) => boolean; 
      errorMessage: string; 
    }[];
  };
}

export const parameterConfigs: Record<string, ParamConfig> = {
  'kyc': {
    context: 'kyc',
    required: ['customerId'],
    optional: ['source'],
    validate: {
      customerId: EC.VALIDATORS.customerId,
      source: EC.VALIDATORS.source
    }
  },
  'prospect/create': {
    context: 'prospect-create',
    optional: ['campaignId', 'source'],
    validate: {
      campaignId: EC.VALIDATORS.campaignId,
      source: EC.VALIDATORS.source
    }
  },
  'prospect/update/:prospectIdOrAccountNumber': {
    context: 'prospect-update',
    required: ['prospectIdOrAccountNumber'],
    optional: ['campaignId', 'source'],
    validate: {
      prospectIdOrAccountNumber: EC.VALIDATORS.prospectIdOrAccountNumber,
      campaignId: EC.VALIDATORS.campaignId,
      source: EC.VALIDATORS.source
    }
  },
  'client-onboarding': {
    context: 'client-onboarding',
    required: ['applicationType'],
    optional: ['referralCode', 'source'],
    validate: {
      applicationType: EC.VALIDATORS.applicationType,
      source: EC.VALIDATORS.source,
      referralCode: {
        validate: (value: string) => /^REF-\d{6}$/.test(value),
        errorMessage: 'Referral code must be in the format REF-XXXXXX where X is a digit'
      }
    }
  },
  'account-summary': {
    context: 'account-summary',
    required: ['accountId'],
    optional: ['fromDate', 'toDate'],
    validate: {
      accountId: EC.VALIDATORS.accountNumber,
      fromDate: {
        validate: (value: string) => !isNaN(Date.parse(value)),
        errorMessage: 'From date must be a valid date'
      },
      toDate: {
        validate: (value: string) => !isNaN(Date.parse(value)),
        errorMessage: 'To date must be a valid date'
      }
    }
  },
  'transaction-history': {
    context: 'transaction-history',
    required: ['accountId', 'fromDate', 'toDate'],
    optional: ['transactionType'],
    validate: {
      accountId: EC.VALIDATORS.accountNumber,
      fromDate: {
        validate: (value: string) => !isNaN(Date.parse(value)),
        errorMessage: 'From date must be a valid date'
      },
      toDate: {
        validate: (value: string) => !isNaN(Date.parse(value)),
        errorMessage: 'To date must be a valid date'
      },
      transactionType: {
        validate: (value: string) => ['credit', 'debit', 'all'].includes(value),
        errorMessage: 'Transaction type must be credit, debit, or all'
      }
    }
  },
  'fund-transfer': {
    context: 'fund-transfer',
    required: ['fromAccount', 'toAccount', 'amount'],
    optional: ['transferType'],
    validate: {
      fromAccount: EC.VALIDATORS.accountNumber,
      toAccount: EC.VALIDATORS.accountNumber,
      amount: {
        validate: (value: string) => /^\d+(\.\d{1,2})?$/.test(value) && parseFloat(value) > 0,
        errorMessage: 'Amount must be a positive number with up to 2 decimal places'
      },
      transferType: {
        validate: (value: string) => ['instant', 'scheduled'].includes(value),
        errorMessage: 'Transfer type must be instant or scheduled'
      }
    }
  }
};

// apps/party/src/app/services/error.service.ts

import { Injectable, signal, computed } from '@angular/core';

export interface ErrorDetails {
  code: string;
  title: string;
  message: string;
  actionText: string;
  actionRoute: string | null;
}

@Injectable({
  providedIn: 'root'
})
export class ErrorService {
  private errorSignal = signal<ErrorDetails | null>(null);

  error = computed(() => this.errorSignal());

  setError(error: ErrorDetails): void {
    this.errorSignal.set(error);
  }

  clearError(): void {
    this.errorSignal.set(null);
  }
}

// apps/party/src/app/error/error.component.ts

import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { ErrorService } from '../services/error.service';

@Component({
  selector: 'app-error',
  standalone: true,
  imports: [CommonModule],
  template: `
    @if (errorService.error(); as error) {
      <div class="error-container">
        <h2>{{ error.title }}</h2>
        <p>{{ error.message }}</p>
        <button (click)="performAction()">{{ error.actionText }}</button>
      </div>
    }
  `,
  styles: [`
    .error-container {
      padding: 20px;
      border: 1px solid #f44336;
      border-radius: 5px;
      background-color: #ffebee;
      color: #f44336;
      text-align: center;
      max-width: 500px;
      margin: 50px auto;
    }
    button {
      margin-top: 20px;
      padding: 10px 20px;
      background-color: #f44336;
      color: white;
      border: none;
      border-radius: 4px;
      cursor: pointer;
    }
    button:hover {
      background-color: #d32f2f;
    }
  `]
})
export class ErrorComponent {
  errorService = inject(ErrorService);
  private router = inject(Router);

  performAction() {
    const error = this.errorService.error();
    if (error?.actionRoute) {
      this.router.navigateByUrl(error.actionRoute);
    } else {
      // Default action if no specific route is provided
      this.router.navigate(['/']);
    }
    this.errorService.clearError();
  }
}

// apps/party/src/app/guards/root-context.guard.ts

import { inject } from '@angular/core';
import { CanActivateFn, Router, ActivatedRouteSnapshot } from '@angular/router';
import { ParameterConfigStore } from '@my-workspace/shared/guards';
import { ErrorService } from '../services/error.service';

export const rootContextGuard: CanActivateFn = (route: ActivatedRouteSnapshot) => {
  const router = inject(Router);
  const paramConfigStore = inject(ParameterConfigStore);
  const errorService = inject(ErrorService);

  const context = route.queryParams['context'];
  
  if (!context) {
    errorService.setError({
      code: 'NO_CONTEXT',
      title: 'Missing Context',
      message: 'No context was provided for the requested operation. Please ensure you're using a valid link or starting from the correct page.',
      actionText: 'Go to Home',
      actionRoute: '/'
    });
    return router.createUrlTree(['/error']);
  }

  const configs = paramConfigStore.configs()();
  const matchingRoute = Object.entries(configs).find(([, config]) => config.context === context);

  if (!matchingRoute) {
    errorService.setError({
      code: 'INVALID_CONTEXT',
      title: 'Invalid Context',
      message: 'The provided context is not valid for any operation. Please check the URL and try again.',
      actionText: 'Go to Home',
      actionRoute: '/'
    });
    return router.createUrlTree(['/error']);
  }

  const [targetPath] = matchingRoute;
  if (route.routeConfig?.path !== targetPath) {
    return router.createUrlTree([`/${targetPath}`], { 
      queryParams: route.queryParams 
    });
  }

  return true;
};

// libs/shared/guards/src/lib/parameter-check.guard.ts

import { inject } from '@angular/core';
import { CanActivateFn, Router, ActivatedRouteSnapshot } from '@angular/router';
import { ParameterConfigStore } from './parameter-config.store';
import { EntitlementConstants as EC } from './entitlement-constants';
import { ErrorService } from '../../apps/party/src/app/services/error.service';

export const parameterCheckGuard: CanActivateFn = (route: ActivatedRouteSnapshot) => {
  const router = inject(Router);
  const paramConfigStore = inject(ParameterConfigStore);
  const errorService = inject(ErrorService);

  const routePath = route.routeConfig?.path || '';
  const paramConfig = paramConfigStore.getConfig()(routePath);
  const queryParams = route.queryParams;

  if (!paramConfig) {
    errorService.setError({
      code: 'INVALID_ROUTE',
      title: 'Invalid Route Configuration',
      message: 'The requested route is not properly configured. Please contact support.',
      actionText: 'Go to Home',
      actionRoute: '/'
    });
    return router.createUrlTree(['/error']);
  }

  // Check required parameters
  if (paramConfig.required) {
    const missingParams = paramConfig.required.filter(param => 
      queryParams[param] === undefined || queryParams[param] === null || queryParams[param] === ''
    );
    if (missingParams.length > 0) {
      errorService.setError({
        code: 'MISSING_PARAMS',
        title: 'Missing Required Parameters',
        message: `The following required parameters are missing: ${missingParams.join(', ')}`,
        actionText: 'Go Back',
        actionRoute: null
      });
      return router.createUrlTree(['/error']);
    }
  }

  // Validate parameters
  if (paramConfig.validate) {
    for (const [param, validator] of Object.entries(paramConfig.validate)) {
      if (queryParams[param] !== undefined) {
        if (Array.isArray(validator)) {
          const isValid = validator.some(v => v.validate(queryParams[param]));
          if (!isValid) {
            errorService.setError({
              code: 'INVALID_PARAM',
              title: 'Invalid Parameter Value',
              message: `Invalid value for parameter: ${param}. ${validator.map(v => v.errorMessage).join(' or ')}`,
              actionText: 'Go Back',
              actionRoute: null
            });
            return router.createUrlTree(['/error']);
          }
        } else {
          if (!validator.validate(queryParams[param])) {
            errorService.setError({
              code: 'INVALID_PARAM',
              title: 'Invalid Parameter Value',
              message: `Invalid value for parameter: ${param}. ${validator.errorMessage}`,
              actionText: 'Go Back',
              actionRoute: null
            });
            return router.createUrlTree(['/error']);
          }
        }
      }
    }
  }

  return true;
};
// libs/shared/guards/src/lib/parameter-check.guard.ts

import { inject } from '@angular/core';
import { CanActivateFn, Router, ActivatedRouteSnapshot } from '@angular/router';
import { ParameterConfigStore } from './parameter-config.store';
import { EntitlementConstants as EC } from './entitlement-constants';
import { ErrorService } from '../../apps/party/src/app/services/error.service';

export const parameterCheckGuard: CanActivateFn = (route: ActivatedRouteSnapshot) => {
  const router = inject(Router);
  const paramConfigStore = inject(ParameterConfigStore);
  const errorService = inject(ErrorService);

  const routePath = route.routeConfig?.path || '';
  const paramConfig = paramConfigStore.getConfig()(routePath);
  const queryParams = route.queryParams;

  if (!paramConfig) {
    errorService.setError({
      code: 'INVALID_ROUTE',
      title: 'Invalid Route Configuration',
      message: 'The requested route is not properly configured. Please contact support.',
      actionText: 'Go to Home',
      actionRoute: '/'
    });
    return router.createUrlTree(['/error']);
  }

  // Check required parameters
  if (paramConfig.required) {
    const missingParams = paramConfig.required.filter(param => 
      queryParams[param] === undefined || queryParams[param] === null || queryParams[param] === ''
    );
    if (missingParams.length > 0) {
      errorService.setError({
        code: 'MISSING_PARAMS',
        title: 'Missing Required Parameters',
        message: `The following required parameters are missing: ${missingParams.join(', ')}`,
        actionText: 'Go Back',
        actionRoute: null
      });
      return router.createUrlTree(['/error']);
    }
  }

  // Validate parameters
  if (paramConfig.validate) {
    for (const [param, validator] of Object.entries(paramConfig.validate)) {
      if (queryParams[param] !== undefined) {
        if (Array.isArray(validator)) {
          const isValid = validator.some(v => v.validate(queryParams[param]));
          if (!isValid) {
            errorService.setError({
              code: 'INVALID_PARAM',
              title: 'Invalid Parameter Value',
              message: `Invalid value for parameter: ${param}. ${validator.map(v => v.errorMessage).join(' or ')}`,
              actionText: 'Go Back',
              actionRoute: null
            });
            return router.createUrlTree(['/error']);
          }
        } else {
          if (!validator.validate(queryParams[param])) {
            errorService.setError({
              code: 'INVALID_PARAM',
              title: 'Invalid Parameter Value',
              message: `Invalid value for parameter: ${param}. ${validator.errorMessage}`,
              actionText: 'Go Back',
              actionRoute: null
            });
            return router.createUrlTree(['/error']);
          }
        }
      }
    }
  }

  return true;
};
