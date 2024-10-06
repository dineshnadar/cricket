// dateUtils.ts

type DateInput = Date | string;

export class DateUtils {
  /**
   * Ensures the input is a Date object.
   * @param date Date object or date string in "YYYY-MM-DD" format
   * @returns Date object
   */
  private static ensureDate(date: DateInput): Date {
    if (typeof date === 'string') {
      return this.parseDate(date);
    }
    return date;
  }

  /**
   * Validates if the given date is a valid birthdate (not in the future).
   * @param date Date object or date string in "YYYY-MM-DD" format
   * @returns True if the date is valid, false otherwise
   */
  static isValidBirthdate(date: DateInput): boolean {
    const birthDate = this.ensureDate(date);
    const today = new Date();
    return birthDate <= today;
  }

  /**
   * Calculates the age based on the given birthdate.
   * @param birthDate Date object or date string in "YYYY-MM-DD" format
   * @returns The calculated age
   * @throws Error if the birthdate is invalid
   */
  static calculateAge(birthDate: DateInput): number {
    const birth = this.ensureDate(birthDate);
    if (!this.isValidBirthdate(birth)) {
      throw new Error("Invalid birthdate: Date is in the future");
    }

    const today = new Date();
    let age = today.getFullYear() - birth.getFullYear();
    const monthDifference = today.getMonth() - birth.getMonth();
    
    if (monthDifference < 0 || (monthDifference === 0 && today.getDate() < birth.getDate())) {
      age--;
    }
    
    return age;
  }

  /**
   * Calculates the next birthday based on the given birthdate.
   * @param birthDate Date object or date string in "YYYY-MM-DD" format
   * @returns The date of the next birthday
   * @throws Error if the birthdate is invalid
   */
  static getNextBirthday(birthDate: DateInput): Date {
    const birth = this.ensureDate(birthDate);
    if (!this.isValidBirthdate(birth)) {
      throw new Error("Invalid birthdate: Date is in the future");
    }

    const today = new Date();
    const nextBirthday = new Date(today.getFullYear(), birth.getMonth(), birth.getDate());
    
    if (nextBirthday < today) {
      nextBirthday.setFullYear(nextBirthday.getFullYear() + 1);
    }
    
    return nextBirthday;
  }

  /**
   * Calculates the days until the next birthday.
   * @param birthDate Date object or date string in "YYYY-MM-DD" format
   * @returns The number of days until the next birthday
   * @throws Error if the birthdate is invalid
   */
  static getDaysUntilNextBirthday(birthDate: DateInput): number {
    const birth = this.ensureDate(birthDate);
    if (!this.isValidBirthdate(birth)) {
      throw new Error("Invalid birthdate: Date is in the future");
    }

    const nextBirthday = this.getNextBirthday(birth);
    const today = new Date();
    const diffTime = Math.abs(nextBirthday.getTime() - today.getTime());
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
    return diffDays;
  }

  /**
   * Formats a date to a string in the format "YYYY-MM-DD".
   * @param date Date object or date string in "YYYY-MM-DD" format
   * @returns The formatted date string
   */
  static formatDate(date: DateInput): string {
    const d = this.ensureDate(date);
    const year = d.getFullYear();
    const month = String(d.getMonth() + 1).padStart(2, '0');
    const day = String(d.getDate()).padStart(2, '0');
    return `${year}-${month}-${day}`;
  }

  /**
   * Parses a date string in the format "YYYY-MM-DD" to a Date object.
   * @param dateString The date string to parse
   * @returns The parsed Date object
   * @throws Error if the date string is invalid
   */
  static parseDate(dateString: string): Date {
    const parts = dateString.split('-');
    if (parts.length !== 3) {
      throw new Error("Invalid date format. Use YYYY-MM-DD.");
    }
    const year = parseInt(parts[0], 10);
    const month = parseInt(parts[1], 10) - 1; // Month is 0-indexed in Date
    const day = parseInt(parts[2], 10);
    const date = new Date(year, month, day);
    if (isNaN(date.getTime())) {
      throw new Error("Invalid date.");
    }
    return date;
  }
}
