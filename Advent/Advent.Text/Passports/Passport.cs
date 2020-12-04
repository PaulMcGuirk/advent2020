using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.Passports
{
    public class Passport
    {
        private Dictionary<string, string> _fields;

        private delegate bool ValidateField(string value);

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

        public void AddData(string data)
        {
            var segments = data.Split(':');
            var key = segments[0];
            var value = segments[1];

            _fields[key] = value;
        }

        public bool HasRequiredFields() => RequiredFields.Keys.All(field => _fields.ContainsKey(field));

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

        public bool IsValid()
        {
            return RequiredFields.All(kvPair =>
            {
                var field = kvPair.Key;
                var validator = kvPair.Value;
                return _fields.ContainsKey(field) && validator(_fields[field]);
            }); 
        }

        private static readonly string[] ValidEyeColors = new string[] { "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };

        private static bool ValidateYear(string value, int minYear, int maxYear)
        {
            if (!int.TryParse(value, out var year))
            {
                return false;
            }

            return minYear <= year && year <= maxYear;
        }

        private static readonly Regex HeightRegex = new Regex(@"^(?<value>\d+)(?<units>in|cm)$");
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
        private static bool ValidateHairColor(string value) => HairColorRegex.IsMatch(value);

        private static bool ValidateEyeColor(string value) => ValidEyeColors.Contains(value);

        private static readonly Regex PassportIDRegex = new Regex("^[0-9]{9}$");
        private static bool ValidatePassportID(string value) => PassportIDRegex.IsMatch(value);
    }
}
