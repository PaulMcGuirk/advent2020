﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.TravelDocuments
{
    public class TrainTicketData
    {

        private Dictionary<string, List<(int min, int max)>> _fieldRanges;
        private List<int> _myTicket;
        private List<List<int>> _nearbyTickets;
        private List<string> _fields;

        private TrainTicketData()
        {
            _fieldRanges = new Dictionary<string, List<(int min, int max)>>();
            _nearbyTickets = new List<List<int>>();
        }

        private static readonly Regex _fieldRegex =
            new Regex(@"^(?<fieldName>[a-z ]+): (?<minOne>\d+)-(?<maxOne>\d+) or (?<minTwo>\d+)-(?<maxTwo>\d+)$");
        /// <summary>
        /// Parse a raw string of ticket data
        /// </summary>
        /// <param name="s">The string to arse</param>
        /// <returns></returns>
        public static TrainTicketData Parse(string s)
        {
            var lines = s.Trim().Split('\n').ToList().GetEnumerator();

            var data = new TrainTicketData();

            // parse field ranges
            while (lines.MoveNext() && !string.IsNullOrEmpty(lines.Current))
            {
                var matches = _fieldRegex.Match(lines.Current);
                if (!matches.Success)
                {
                    throw new Exception($"Expected: field ranges. Actual: for {lines.Current}");
                }

                var fieldName = matches.Groups["fieldName"].Value;
                var minOne = int.Parse(matches.Groups["minOne"].Value);
                var maxOne = int.Parse(matches.Groups["maxOne"].Value);
                var minTwo = int.Parse(matches.Groups["minTwo"].Value);
                var maxTwo = int.Parse(matches.Groups["maxTwo"].Value);

                data._fieldRanges[fieldName] = new List<(int min, int max)> { (minOne, maxOne), (minTwo, maxTwo) };
            }

            lines.MoveNext(); // "your ticket:"
            lines.MoveNext();
            data._myTicket = lines.Current.Split(',').Select(int.Parse).ToList();

            lines.MoveNext(); // expected blank
            lines.MoveNext(); // "nearby ticket"

            // parse remaining tickets
            while (lines.MoveNext())
            {
                
                data._nearbyTickets.Add(lines.Current.Split(',').Select(int.Parse).ToList());
            }

            return data;
        }

        /// <summary>
        /// Get the scanning error rate for these data
        /// </summary>
        /// <returns>The scanning error rate</returns>
        public int GetScanningErrorRate()
            => _nearbyTickets.Select(ticket => ticket.Where(IsValueInvalidForAllFields).Sum()).Sum();

        /// <summary>
        /// Determine if the field value is invalid for all fields
        /// </summary>
        /// <param name="fieldValue">The value to check</param>
        /// <returns><c>true</c> if there are no fields for which this value is valid</returns>
        private bool IsValueInvalidForAllFields(int fieldValue)
            => !_fieldRanges.Keys.Any(fieldName => FieldContainsValue(fieldName, fieldValue));

        /// <summary>
        /// Return true if the field can contain the value in any of the allowed ranges
        /// </summary>
        /// <param name="fieldName">The name of the field</param>
        /// <param name="value">The value to check</param>
        /// <returns>true if the field con contain this value</returns>
        private bool FieldContainsValue(string fieldName, int value)
            => _fieldRanges[fieldName].Any(pair => pair.min <= value && value <= pair.max);

        /// <summary>
        /// Identify which position in the ticket corresponds to which field
        /// </summary>
        public void IdentifyFields()
        {
            var validTickets = _nearbyTickets.Where(ticket => !ticket.Any(IsValueInvalidForAllFields)).ToList();

            var numFields = _fieldRanges.Count;
            var possibleFields = new List<HashSet<string>>();

            // based on all the field values, narrow down the list of fields
            for (var i = 0; i < numFields; i++)
            {
                possibleFields.Add(validTickets
                    .Select(ticket => _fieldRanges.Keys.Where(fieldName => FieldContainsValue(fieldName, ticket[i])).ToHashSet())
                    .Aggregate(new HashSet<string>(_fieldRanges.Keys), (result, next) => result.Intersect(next).ToHashSet()));
            }

            // if any slot has been reduced to one field, we can use that field
            // to narrow down the others
            while (true)
            {
                var done = true;
                for (var i = 0; i < numFields; i++)
                {
                    if (possibleFields[i].Count > 1)
                    {
                        done = false;
                        continue;
                    }

                    var field = possibleFields[i].First();
                    for (var j = 0; j < numFields; j++)
                    {
                        if (i != j)
                        {
                            possibleFields[j].Remove(field);
                        }
                    }
                }
                if (done)
                {
                    break;
                }
            }

            _fields = possibleFields.Select(x => x.Single()).ToList();
        }

        /// <summary>
        /// Get the departure summary for your ticket. This is just the product
        /// of the values of fields with the word "departure" in them.
        /// </summary>
        /// <returns>The departure summary</returns>
        public long GetDepartureSummary()
            => GetDepartureSummary(_myTicket);

        /// <summary>
        /// Get the departure summary for a ticket. This is just the product
        /// of the values of fields with the word "departure" in them.
        /// </summary>
        /// <param name="ticket">The ticket to the get the summary for</param>
        /// <returns>The departure summary</returns>
        private long GetDepartureSummary(List<int> ticket)
            => ticket.Select((value, i) => (field: _fields[i], value))
                .Where(pair => pair.field.Contains("departure"))
                .Aggregate(1L, (product, pair) => product * pair.value);
    }
}
