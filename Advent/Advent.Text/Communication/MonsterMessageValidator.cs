using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Communication
{
    public class MonsterMessageValidator
    {
        private readonly Rule[] _rules;

        /// <summary>
        /// Create a new monster message validator.
        /// </summary>
        /// <param name="ruleStrings">The strings representing the rules</param>
        public MonsterMessageValidator(List<string> ruleStrings)
        {
            _rules = new Rule[ruleStrings.Count];
            foreach (var ruleString in ruleStrings)
            {
                var pieces = ruleString.Split(": ");
                var id = int.Parse(pieces[0]);
                var rule = new Rule(pieces[1]);

                _rules[id] = rule;
            }
        }

        /// <summary>
        /// Check if the given string is valid according to the rules,
        /// starting with rule 0.
        /// </summary>
        /// <returns><c>true</c> if the rule is valid, <c>false</c> otherwise</returns>
        public bool Check(string s)
            => CheckHelper(s, 0, 0).Any(pos => pos == s.Length);

        /// <summary>
        /// Does the heavy lifting to run the check. This checks the input
        /// string against the specified rule. Since there are multiple ways
        /// that a rule could be satisfied, it's not sufficient to just return
        /// a true/false. Instead, for each way that the string satisfies this
        /// rule, this will generate the position after that the substring
        /// that satisfies the rule. If the string doesn't satisfy the rule,
        /// this is an empty enumeator.
        /// </summary>
        /// <param name="s">The string to check</param>
        /// <param name="ruleId">The rule number to check</param>
        /// <param name="stringPos">Where in the string to start checking</param>
        /// <returns></returns>
        private IEnumerable<int> CheckHelper(string s, int ruleId, int stringPos)
        {
            var rule = _rules[ruleId];

            if (rule.Type == RuleTypes.Base)
            {
                // if check char is populated, this is a simple check
                if (rule.CheckChar == s[stringPos])
                {
                    yield return stringPos + 1;
                }
                yield break;
            }

            // for nested rules, we may check each branch
            foreach (var branch in rule.Branches)
            {
                // since each nested rules might generate a different position,
                // we need to track where the rules bring us back.
                // using a stack here means that we're going depth first
                var toCheck = new Stack<(int rulePos, int stringPos)>();
                var visited = new HashSet<(int rulePos, int stringPos)>();
                toCheck.Push((0, stringPos));

                while (toCheck.TryPop(out var node))
                {
                    if (visited.Contains(node))
                    {
                        continue;
                    }
                    visited.Add(node);

                    var (rulePos, pos) = node;

                    if (rulePos >= branch.Count)
                    {
                        // if we're out of nested rules, then the whole rule is satisfied
                        yield return pos;
                        continue;
                    }
                    if (pos >= s.Length)
                    {
                        continue; // string is too long, the rule is not satisfied
                    }

                    var nextRuleId = branch[rulePos];

                    // look for each way that the nested rule is satisfied
                    // If the nested rule is not satisfied, then this
                    // enumerator will be empty, and so nothing new will
                    // be pushed on the stack. I.e., this particular search dies out.
                    foreach (var newPos in CheckHelper(s, nextRuleId, pos))
                    {
                        toCheck.Push((rulePos + 1, newPos));
                    }
                }
            }
        }

        /// <summary>
        /// This can either be a "base" rule, meaning checks against a given
        /// character, or a nested rule. A nested rule consists of a list
        /// of branches, at least one of which needs to be satisfied.
        /// Each branch consists of a list of rules, each of which must be
        /// satisfied for that branch to be satisfied.
        /// </summary>
        private class Rule
        {
            public RuleTypes Type { get; }
            public char CheckChar { get; }
            public List<List<int>> Branches { get; }

            /// <summary>
            /// Parse a rule
            /// </summary>
            /// <param name="s">The string to parse</param>
            public Rule(string s)
            {
                if (s[0] == '"')
                {
                    Type = RuleTypes.Base;
                    CheckChar = s[1];
                    return;
                }

                Type = RuleTypes.Nested;

                Branches = s.Split('|')
                    .Select(branchString =>
                        branchString.Trim().Split(' ')
                            .Select(int.Parse)
                            .ToList())
                    .ToList();
            }
        }

        private enum RuleTypes
        {
            Base,
            Nested
        }
    }

}
