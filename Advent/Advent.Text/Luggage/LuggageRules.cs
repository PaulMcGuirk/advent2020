using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Luggage
{
    public class LuggageRules
    {
        /// <summary>
        /// The rules, keyed by parent ID
        /// </summary>
        public Dictionary<string, LuggageRule> Rules;

        /// <summary>
        /// Parse a rules string. A rules string is a newline-delimited list
        /// of rule string.
        /// </summary>
        /// <param name="rulesString">The rules string to parse</param>
        /// <returns>The rules</returns>
        public static LuggageRules Parse(string rulesString)
        {
            var rules = rulesString.Trim().Split("\n")
                .Select(LuggageRule.Parse)
                .ToDictionary(rule => rule.Color);

            foreach (var rule in rules.Values)
            {
                foreach (var child in rule.Children.Keys)
                {
                    rules[child].Parents.Add(rule.Color);
                }
            }

            return new LuggageRules { Rules = rules };
        }

        /// <summary>
        /// Count all the number of colors that can contain this bag.
        /// </summary>
        /// <param name="colorName">The color to get the ancesotrs for</param>
        /// <returns>The number of colors of bags that can contain this bag</returns>
        public int CountAncestors(string colorName)
        {
            var ancestorsToVisit = new Stack<string>(Rules[colorName].Parents);
            var ancestors = new HashSet<string>();

            while (ancestorsToVisit.TryPop(out var ancestor))
            {
                if (ancestors.Contains(ancestor))
                {
                    continue;
                }
                ancestors.Add(ancestor);
                Rules[ancestor].Parents.ForEach(ancestorsToVisit.Push);
            }

            return ancestors.Count;
        }

        /// <summary>
        /// Given the name of a bag, count the number of bags that this bag
        /// would contain.
        /// </summary>
        /// <param name="colorName">The color of bag</param>
        /// <returns>The number of bags that this would contain</returns>
        public int CountChildrenWithQuantity(string colorName)
        {
            var counts = new Dictionary<string, int>();
            var toVisit = new Stack<string>();
            toVisit.Push(colorName);

            while (!counts.ContainsKey(colorName) && toVisit.TryPeek(out var innerBagColor))
            {
                if (counts.ContainsKey(innerBagColor))
                {
                    toVisit.Pop();
                    continue;
                }

                var rule = Rules[innerBagColor];

                var childrenToVisit = rule.Children.Keys
                    .Where(child => !counts.ContainsKey(child));
                if (childrenToVisit.Any())
                {
                    // if any children aren't yet counted, we need to
                    // process them first
                    foreach (var child in childrenToVisit)
                    {
                        toVisit.Push(child);
                    }
                    
                    continue;
                }

                toVisit.Pop(); // if all children are processed, we can get the total for this bag

                counts[innerBagColor] = rule.Children
                    .Select(kvPair => kvPair.Value * (1 + counts[kvPair.Key]))
                    .Sum();
            }

            return counts[colorName];
        }
    }
}
