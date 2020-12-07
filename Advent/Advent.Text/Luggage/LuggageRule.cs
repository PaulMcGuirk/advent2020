using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Advent.Text.Luggage
{
    public class LuggageRule
    {
        /// <summary>
        /// The color of the bag that this rule applies to
        /// </summary>
        public string Color { get; }

        /// <summary>
        /// The colors of backs that must be contained in this bag and the
        /// number of bags that must be fit
        /// </summary>
        public Dictionary<string, int> Children { get; }

        /// <summary>
        /// The colors of bags that can contain this bag
        /// </summary>
        public List<string> Parents { get; }

        /// <summary>
        /// Contain a new, empty rule.
        /// </summary>
        /// <param name="color"></param>
        public LuggageRule(string color)
        {
            Color = color;
            Children = new Dictionary<string, int>();
            Parents = new List<string>();
        }

        private static readonly Regex _ruleRegex = new Regex(@"^(?<bagColor>[a-z ]+) bags contain (?<children>[a-z0-9, ]+).$");
        private static readonly Regex _childRegex = new Regex(@"^(?<amount>\d+) (?<color>[a-z ]+) bag(s|)$");
        private const string NO_CHILDREN_STRING = "no other bags";
        /// <summary>
        /// Parse a rule string.
        /// </summary>
        public static LuggageRule Parse(string ruleString)
        { 
            var match = _ruleRegex.Match(ruleString);
            if (match == null)
            {
                return null;
            }

            var bagColor = match.Groups["bagColor"].Value;
            var rule = new LuggageRule(bagColor);

            var children = match.Groups["children"].Value;
            if (children == NO_CHILDREN_STRING)
            {
                return rule;
            }

            foreach (var child in children.Split(','))
            {
                var childMatch = _childRegex.Match(child.Trim());
                var amount = int.Parse(childMatch.Groups["amount"].Value);
                var childColor = childMatch.Groups["color"].Value;

                rule.Children[childColor] = amount;
            }

            return rule;
        }
    }
}
