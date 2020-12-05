using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.TravelDocuments
{
    public class Passport
    {
        /// <summary>
        /// Passport data
        /// </summary>
        private Dictionary<string, string> _fields;

        /// <summary>
        /// Field-specific validator
        /// </summary>
        private delegate bool ValidateField(string value);

        /// <summary>
        /// The required fields and their validation rules
        /// </summary>
        private static readonly Dictionary<string, ValidateField> RequiredFields = new Dictionary<string, ValidateField>
        {
            ["byr"] = value => ValidateYear(value, 1920, 2002),
            ["iyr"] = value => ValidateYear(value, 2010, 2020),
            ["eyr"] = value => ValidateYear(value, 2020, 2030),
            ["hgt"] = ValidateHeight,
            ["hcl"] = ValidateHairColor,
            ["ecl"] = ValidateEyeColor,
            ["pid"] = ValidatePassportID
        };

        public Passport()
        {
            _fields = new Dictionary<string, string>();
        }

        /// <summary>
        /// Add a key-value pair of the form "key:value"
        /// </summary>
        public void AddData(string data)
        {
            var segments = data.Split(':');
            var key = segments[0];
            var value = segments[1];

            _fields[key] = value;
        }

        /// <summary>
        /// Parse a batch string of passport data. The data for each passport
        /// is one or more lines of key-value pairs separated by spaces. Each
        /// key-value paired is of the form key:value. A blank line indicates
        /// the end of the data for a passport.
        /// </summary>
        /// <param name="batchString"></param>
        /// <returns></returns>
        public static List<Passport> ParseBatchString(string batchString)
        {
            var lines = batchString.Split('\n');
            var passport = new Passport();
            var passports = new List<Passport>();

            foreach (var line in lines)
            {
                if (string.IsNullOrEmpty(line))
                {
                    // the last line in a batch file is blank, so ths will
                    // capture the last one
                    passports.Add(passport);
                    passport = new Passport();
                    continue;
                }

                var fieldData = line.Split(' ');
                foreach (var fieldDatum in fieldData)
                {
                    passport.AddData(fieldDatum);
                }
            }

            return passports;
        }

        /// <summary>
        /// Check if all required fields are present
        /// </summary>
        public bool HasRequiredFields() => RequiredFields.Keys.All(field => _fields.ContainsKey(field));

        /// <summary>
        /// Check if all required fields are present and have valid values
        /// </summary>
        public bool IsValid()
        {
            return RequiredFields.All(kvPair =>
            {
                var field = kvPair.Key;
                var validator = kvPair.Value;
                return _fields.ContainsKey(field) && validator(_fields[field]);
            }); 
        }

        #region Validators

        /// <summary>
        /// A year is an integer bounded by two values
        /// </summary>
        private static bool ValidateYear(string value, int minYear, int maxYear)
        {
            if (!int.TryParse(value, out var year))
            {
                return false;
            }

            return minYear <= year && year <= maxYear;
        }

        private static readonly Regex HeightRegex = new Regex(@"^(?<value>\d+)(?<units>in|cm)$");
        /// <summary>
        /// The height must be measured in "cm" or "in" and must have values
        /// in a specific range
        /// </summary>
        private static bool ValidateHeight(string value)
        {
            var match = HeightRegex.Match(value);
            if (match == null || !match.Success)
            {
                return false;
            }

            var units = match.Groups["units"].Value;
            var heightValue = int.Parse(match.Groups["value"].Value);

            if (units == "in")
            {
                return 59 <= heightValue && heightValue <= 76;
            }
            return 150 <= heightValue && heightValue <= 193;

        }

        private static readonly Regex HairColorRegex = new Regex("^#[0-9a-f]{6}$");
        /// <summary>
        /// Hair color must be a valid hex format color (using lower case)
        /// </summary>
        private static bool ValidateHairColor(string value) => HairColorRegex.IsMatch(value);

        private static readonly string[] ValidEyeColors = new string[] { "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
        /// <summary>
        /// Eye color must be on a specific list
        /// </summary>
        private static bool ValidateEyeColor(string value) => ValidEyeColors.Contains(value);

        private static readonly Regex PassportIDRegex = new Regex("^[0-9]{9}$");
        /// <summary>
        /// The passport ID must be 9 digits (including leading zeroes)
        /// </summary>
        private static bool ValidatePassportID(string value) => PassportIDRegex.IsMatch(value);

        #endregion
    }
}
