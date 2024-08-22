private setFormLayout() {
  const layout: FormLayout = {
    type: 'multipleSection',
    sections: [
      {
        type: 'leftHeader',
        leftHeader: 'Personal Information',
        fields: [
          { name: 'personalInfo.firstName', side: 'left', seq: 1 },
          { name: 'personalInfo.lastName', side: 'left', seq: 2 },
          { name: 'email', side: 'right', seq: 3 },
          { name: 'phone', side: 'right', seq: 4 },
          { name: 'dateOfBirth', side: 'left', seq: 5 },
          { name: 'gender', side: 'right', seq: 6 }
        ]
      },
      {
        type: 'leftRightHeader',
        leftHeader: 'Address',
        rightHeader: 'Details',
        fields: [
          { name: 'address.street', side: 'left', seq: 1 },
          { name: 'address.city', side: 'left', seq: 2 },
          { name: 'address.state', side: 'right', seq: 3 },
          { name: 'address.zipCode', side: 'right', seq: 4 },
          { name: 'address.country', side: 'left', seq: 5 }
        ]
      },
      {
        type: 'accordion',
        accordionHeader: 'Employment Information',
        fields: [
          { name: 'employment.currentlyEmployed', side: 'left', seq: 1 },
          { name: 'employment.employer', side: 'left', seq: 2 },
          { name: 'employment.position', side: 'right', seq: 3 },
          { name: 'employment.startDate', side: 'left', seq: 4 },
          { name: 'employment.salary', side: 'right', seq: 5 }
        ]
      },
      {
        type: 'commonHeader',
        commonHeader: 'Preferences',
        fields: [
          { name: 'preferences.newsletter', side: 'left', seq: 1 },
          { name: 'preferences.theme', side: 'right', seq: 2 },
          { name: 'preferences.notifications.email', side: 'left', seq: 3 },
          { name: 'preferences.notifications.sms', side: 'left', seq: 4 },
          { name: 'preferences.notifications.push', side: 'right', seq: 5 }
        ]
      }
    ]
  };
  this.formExtensionService.setFormGroupLayout(this.profileForm, layout);
}


----------------------

  import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';
import { FormExtensionService, FormLayout } from './form-extension.service';

@Component({
  selector: 'app-user-profile',
  template: `
    <form [formGroup]="profileForm">
      <!-- Form fields would be rendered here based on the layout -->
    </form>
    <h2>Extended Read View</h2>
    <pre>{{ extendedReadView() | json }}</pre>
  `
})
export class UserProfileComponent implements OnInit {
  profileForm: FormGroup;
  extendedReadView = this.formExtensionService.getExtendedReadView(this.profileForm);

  constructor(
    private fb: FormBuilder,
    private formExtensionService: FormExtensionService
  ) {}

  ngOnInit() {
    this.initForm();
    this.extendFormControls();
    this.setFormLayout();
    this.registerCustomComputations();
    this.formExtensionService.applyDefaultValues(this.profileForm, true);
  }

  private initForm() {
    this.profileForm = this.fb.group({
      personalInfo: this.fb.group({
        firstName: ['', Validators.required],
        lastName: ['', Validators.required],
      }),
      email: ['', [Validators.required, Validators.email]],
      phone: [''],
      dateOfBirth: ['', Validators.required],
      gender: [''],
      address: this.fb.group({
        street: [''],
        city: [''],
        state: [''],
        zipCode: [''],
        country: ['']
      }),
      employment: this.fb.group({
        currentlyEmployed: [false],
        employer: [''],
        position: [''],
        startDate: [''],
        salary: ['']
      }),
      education: this.fb.array([]),
      skills: this.fb.array([]),
      preferences: this.fb.group({
        newsletter: [false],
        theme: ['light'],
        notifications: this.fb.group({
          email: [true],
          sms: [false],
          push: [true]
        })
      }),
      securityQuestions: this.fb.array([
        this.fb.group({
          question: [''],
          answer: ['']
        }),
        this.fb.group({
          question: [''],
          answer: ['']
        })
      ])
    });
  }

  private extendFormControls() {
    this.formExtensionService.batchUpdates(() => {
      // Personal Info
      this.formExtensionService.extendControl(this.profileForm.get('personalInfo.firstName'), {
        label: 'First Name',
        fldName: 'firstName',
        required: true,
        sectionName: 'Personal Information'
      });
      this.formExtensionService.extendControl(this.profileForm.get('personalInfo.lastName'), {
        label: 'Last Name',
        fldName: 'lastName',
        required: true,
        sectionName: 'Personal Information'
      });
      this.formExtensionService.extendControl(this.profileForm.get('email'), {
        label: 'Email',
        fldName: 'email',
        required: true,
        sectionName: 'Personal Information',
        regex: ['^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$']
      });
      this.formExtensionService.extendControl(this.profileForm.get('phone'), {
        label: 'Phone',
        fldName: 'phone',
        sectionName: 'Personal Information'
      });
      this.formExtensionService.extendControl(this.profileForm.get('dateOfBirth'), {
        label: 'Date of Birth',
        fldName: 'dateOfBirth',
        required: true,
        sectionName: 'Personal Information',
        fieldType: 'date'
      });
      this.formExtensionService.extendControl(this.profileForm.get('gender'), {
        label: 'Gender',
        fldName: 'gender',
        sectionName: 'Personal Information',
        lookupData: [
          { label: 'Male', value: 'male' },
          { label: 'Female', value: 'female' },
          { label: 'Other', value: 'other' }
        ]
      });

      // Address
      this.formExtensionService.extendControl(this.profileForm.get('address.street'), {
        label: 'Street',
        fldName: 'street',
        sectionName: 'Address'
      });
      this.formExtensionService.extendControl(this.profileForm.get('address.city'), {
        label: 'City',
        fldName: 'city',
        sectionName: 'Address'
      });
      this.formExtensionService.extendControl(this.profileForm.get('address.state'), {
        label: 'State',
        fldName: 'state',
        sectionName: 'Address'
      });
      this.formExtensionService.extendControl(this.profileForm.get('address.zipCode'), {
        label: 'Zip Code',
        fldName: 'zipCode',
        sectionName: 'Address'
      });
      this.formExtensionService.extendControl(this.profileForm.get('address.country'), {
        label: 'Country',
        fldName: 'country',
        sectionName: 'Address',
        lookupData: [
          { label: 'United States', value: 'US' },
          { label: 'Canada', value: 'CA' },
          { label: 'United Kingdom', value: 'UK' }
        ]
      });

      // Employment
      this.formExtensionService.extendControl(this.profileForm.get('employment.currentlyEmployed'), {
        label: 'Currently Employed',
        fldName: 'currentlyEmployed',
        sectionName: 'Employment',
        fieldType: 'checkbox'
      });
      this.formExtensionService.extendControl(this.profileForm.get('employment.employer'), {
        label: 'Employer',
        fldName: 'employer',
        sectionName: 'Employment'
      });
      this.formExtensionService.extendControl(this.profileForm.get('employment.position'), {
        label: 'Position',
        fldName: 'position',
        sectionName: 'Employment'
      });
      this.formExtensionService.extendControl(this.profileForm.get('employment.startDate'), {
        label: 'Start Date',
        fldName: 'startDate',
        sectionName: 'Employment',
        fieldType: 'date'
      });
      this.formExtensionService.extendControl(this.profileForm.get('employment.salary'), {
        label: 'Salary',
        fldName: 'salary',
        sectionName: 'Employment',
        fieldType: 'number'
      });

      // Preferences
      this.formExtensionService.extendControl(this.profileForm.get('preferences.newsletter'), {
        label: 'Subscribe to Newsletter',
        fldName: 'newsletter',
        sectionName: 'Preferences',
        fieldType: 'checkbox'
      });
      this.formExtensionService.extendControl(this.profileForm.get('preferences.theme'), {
        label: 'Preferred Theme',
        fldName: 'theme',
        sectionName: 'Preferences',
        lookupData: [
          { label: 'Light', value: 'light' },
          { label: 'Dark', value: 'dark' }
        ]
      });
      this.formExtensionService.extendControl(this.profileForm.get('preferences.notifications.email'), {
        label: 'Email Notifications',
        fldName: 'emailNotifications',
        sectionName: 'Preferences',
        subSectionName: 'Notifications',
        fieldType: 'checkbox'
      });
      this.formExtensionService.extendControl(this.profileForm.get('preferences.notifications.sms'), {
        label: 'SMS Notifications',
        fldName: 'smsNotifications',
        sectionName: 'Preferences',
        subSectionName: 'Notifications',
        fieldType: 'checkbox'
      });
      this.formExtensionService.extendControl(this.profileForm.get('preferences.notifications.push'), {
        label: 'Push Notifications',
        fldName: 'pushNotifications',
        sectionName: 'Preferences',
        subSectionName: 'Notifications',
        fieldType: 'checkbox'
      });
    });
  }

  private setFormLayout() {
    const layout: FormLayout = {
      type: 'multipleSection',
      sections: [
        {
          type: 'leftHeader',
          leftHeader: 'Personal Information',
          fields: [
            { name: 'personalInfo.firstName', side: 'left' },
            { name: 'personalInfo.lastName', side: 'left' },
            { name: 'email', side: 'right' },
            { name: 'phone', side: 'right' },
            { name: 'dateOfBirth', side: 'left' },
            { name: 'gender', side: 'right' }
          ]
        },
        {
          type: 'leftRightHeader',
          leftHeader: 'Address',
          rightHeader: 'Details',
          fields: [
            { name: 'address.street', side: 'left' },
            { name: 'address.city', side: 'left' },
            { name: 'address.state', side: 'right' },
            { name: 'address.zipCode', side: 'right' },
            { name: 'address.country', side: 'left' }
          ]
        },
        {
          type: 'accordion',
          accordionHeader: 'Employment Information',
          fields: [
            { name: 'employment.currentlyEmployed', side: 'left' },
            { name: 'employment.employer', side: 'left' },
            { name: 'employment.position', side: 'right' },
            { name: 'employment.startDate', side: 'left' },
            { name: 'employment.salary', side: 'right' }
          ]
        },
        {
          type: 'commonHeader',
          commonHeader: 'Preferences',
          fields: [
            { name: 'preferences.newsletter', side: 'left' },
            { name: 'preferences.theme', side: 'right' },
            { name: 'preferences.notifications.email', side: 'left' },
            { name: 'preferences.notifications.sms', side: 'left' },
            { name: 'preferences.notifications.push', side: 'right' }
          ]
        }
      ]
    };
    this.formExtensionService.setFormGroupLayout(this.profileForm, layout);
  }

  private registerCustomComputations() {
    // Compute full name
    this.formExtensionService.registerCustomComputation(
      this.profileForm.get('personalInfo'),
      (control, form) => {
        const firstName = form.get('personalInfo.firstName')?.value;
        const lastName = form.get('personalInfo.lastName')?.value;
        return `${firstName} ${lastName}`;
      }
    );

    // Compute employment status
    this.formExtensionService.registerCustomComputation(
      this.profileForm.get('employment'),
      (control, form) => {
        const currentlyEmployed = form.get('employment.currentlyEmployed')?.value;
        const employer = form.get('employment.employer')?.value;
        return currentlyEmployed ? `Employed at ${employer}` : 'Not currently employed';
      }
    );
  }

  // Additional methods for handling dynamic form arrays (education, skills, security questions)
  addEducation() {
    const education = this.profileForm.get('education') as FormArray;
    const newEducation = this.fb.group({
      degree: [''],
      institution: [''],
      year: ['']
    });
    education.push(newEducation);
    this.extendEducationControl(newEducation, education.length - 1);
  }

  addSkill() {
    const skills = this.profileForm.get('skills') as FormArray;
    const newSkill = this.fb.control('');
    skills.push(newSkill);
    this.extendSkillControl(newSkill, skills.length - 1);
  }

  private extendEducationControl(control: FormGroup, index: number) {
    this.formExtensionService.extendControl(control.get('degree'), {
      label: `Degree ${index + 1}`,
      fldName: `degree_${index}`,
      sectionName: 'Education'
    });
    this.formExtensionService.extendControl(control.get('institution'), {
      label: `Institution ${index + 1}`,
      fldName: `institution_${index}`,
      sectionName: 'Education'
    });
    this.formExtensionService.extendControl(control.get('year'), {
      label: `Year ${index + 1}`,
      fldName: `year_${index}`,
      sectionName: 'Education',
      fieldType: 'number'
    });
  }

  private extendSkillControl(control: FormGroup, index: number) {
    this.formExtensionService.extendControl(control, {
      label: `Skill ${index + 1}`,
      fldName: `skill_${index}`,
      sectionName: 'Skills'
    });
  }
}
