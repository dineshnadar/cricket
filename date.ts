// dateUtils.ts

export class DateUtils {
  /**
   * Validates if the given date is a valid birthdate (not in the future).
   * @param date The date to validate
   * @returns True if the date is valid, false otherwise
   */
  static isValidBirthdate(date: Date): boolean {
    const today = new Date();
    return date <= today;
  }

  /**
   * Calculates the age based on the given birthdate.
   * @param birthDate The date of birth
   * @returns The calculated age
   * @throws Error if the birthdate is invalid
   */
  static calculateAge(birthDate: Date): number {
    if (!this.isValidBirthdate(birthDate)) {
      throw new Error("Invalid birthdate: Date is in the future");
    }

    const today = new Date();
    let age = today.getFullYear() - birthDate.getFullYear();
    const monthDifference = today.getMonth() - birthDate.getMonth();
    
    if (monthDifference < 0 || (monthDifference === 0 && today.getDate() < birthDate.getDate())) {
      age--;
    }
    
    return age;
  }

  /**
   * Calculates the next birthday based on the given birthdate.
   * @param birthDate The date of birth
   * @returns The date of the next birthday
   * @throws Error if the birthdate is invalid
   */
  static getNextBirthday(birthDate: Date): Date {
    if (!this.isValidBirthdate(birthDate)) {
      throw new Error("Invalid birthdate: Date is in the future");
    }

    const today = new Date();
    const nextBirthday = new Date(today.getFullYear(), birthDate.getMonth(), birthDate.getDate());
    
    if (nextBirthday < today) {
      nextBirthday.setFullYear(nextBirthday.getFullYear() + 1);
    }
    
    return nextBirthday;
  }

  /**
   * Calculates the days until the next birthday.
   * @param birthDate The date of birth
   * @returns The number of days until the next birthday
   * @throws Error if the birthdate is invalid
   */
  static getDaysUntilNextBirthday(birthDate: Date): number {
    if (!this.isValidBirthdate(birthDate)) {
      throw new Error("Invalid birthdate: Date is in the future");
    }

    const nextBirthday = this.getNextBirthday(birthDate);
    const today = new Date();
    const diffTime = Math.abs(nextBirthday.getTime() - today.getTime());
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
    return diffDays;
  }

  /**
   * Formats a date to a string in the format "YYYY-MM-DD".
   * @param date The date to format
   * @returns The formatted date string
   */
  static formatDate(date: Date): string {
    const year = date.getFullYear();
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
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
