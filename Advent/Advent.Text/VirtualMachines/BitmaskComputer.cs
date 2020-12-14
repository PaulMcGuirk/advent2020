using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.VirtualMachines
{

    /// <summary>
    /// How the masking is handled
    /// </summary>
    public enum BitmaskingTypes
    {
        /// <summary>
        /// Values are masked before being written to memroy
        /// </summary>
        Value,
        /// <summary>
        /// Memory addressess are masked a value is written to memory
        /// </summary>
        Address
    }

    public class BitmaskComputer
    {

        private const int ADDRESS_SIZE = 36;
        private BitmaskingTypes _maskingType;

        // model memory with a dictionary - probably most of memory will
        // be empty so this will save real memory and time
        private Dictionary<long, long> _memory;

        /// <summary>
        /// The current bit mask
        /// </summary>
        private char[] _bitMask;

        /// <summary>
        /// Create a new bitmask computer, using value masking
        /// </summary>
        public BitmaskComputer() : this(BitmaskingTypes.Value)
        {

        }

        /// <summary>
        /// Create a new bitmask computer with the specified masking type
        /// </summary>
        /// <param name="maskingType">The type of masking to use</param>
        public BitmaskComputer(BitmaskingTypes maskingType)
        {
            _memory = new Dictionary<long, long>();
            _bitMask = Enumerable.Repeat('X', ADDRESS_SIZE).ToArray();
            _maskingType = maskingType;
        }

        /// <summary>
        /// Run a set of newline delimited instructions
        /// </summary>
        /// <param name="instructions">The instructions to run</param>
        public void RunInstructions(string instructions)
        {
            var lines = instructions.Trim().Split('\n');
            foreach (var line in lines)
            {
                RunInstruction(line);
            }
        }

        /// <summary>
        /// Get the total of all values stored in memory
        /// </summary>
        public long TotalValuesInMemory() => _memory.Values.Sum();

        private static readonly Regex _maskRegex = new Regex(@"^mask = (?<mask>[01X]{36})$");
        private static readonly Regex _setRegex = new Regex(@"^mem\[(?<address>\d+)\] = (?<value>\d+)$");
        private void RunInstruction(string instruction)
        {
            var match = _maskRegex.Match(instruction);
            if (match.Success)
            {
                // update the mask
                _bitMask = match.Groups["mask"].Value.ToArray();
                return;
            }

            match = _setRegex.Match(instruction);
            if (match.Success)
            {
                // write to memory
                var address = long.Parse(match.Groups["address"].Value);
                var value = long.Parse(match.Groups["value"].Value);
                if (_maskingType == BitmaskingTypes.Value)
                {
                    _memory[address] = MaskValue(value);
                }
                else
                {
                    foreach (var maskedAddress in MaskAddress(address))
                    {
                        _memory[maskedAddress] = value;
                    }
                }
                return;
            }

            throw new InvalidOperationException($"Invalid instruction {instruction}");
        }

        /// <summary>
        /// Apply the mask to the given value
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private long MaskValue(long value)
        {
            var bits = new string(Convert.ToString(value, 2)
                .PadLeft(ADDRESS_SIZE, '0')
                .Select((ch, pos) => _bitMask[pos] == 'X' ? ch : _bitMask[pos])
                .ToArray());

            return Convert.ToInt64(bits, 2);
        }

        /// <summary>
        /// Apply the mask to the given address. This returns multiple
        /// addresses
        /// </summary>
        /// <param name="address">The address to mask</param>
        /// <returns>The resulting masked addresses</returns>
        private IEnumerable<long> MaskAddress(long address)
        {
            var addressBits = new string(Convert.ToString(address, 2)).PadLeft(ADDRESS_SIZE, '0');

            foreach (var maskedAddress in MaskAddressHelper(addressBits, new List<char>(), 0))
            {
                yield return maskedAddress;
            }

            // helper function for the recursion
            IEnumerable<long> MaskAddressHelper(string addressBits, List<char> working, int pos)
            {
                if (pos == ADDRESS_SIZE)
                {
                    yield return Convert.ToInt64(new string(working.ToArray()), 2);
                    yield break;
                }

                var ch = _bitMask[pos];

                if (ch == 'X')
                {
                    // for a floating bit, we consider both adding 0 and adding 1
                    var newWorking = new List<char>(working) { '0' };
                    foreach (var maskedAddress in MaskAddressHelper(addressBits, newWorking, pos + 1))
                    {
                        yield return maskedAddress;
                    }
                    working.Add('1');
                    foreach (var maskedAddress in MaskAddressHelper(addressBits, working, pos + 1))
                    {
                        yield return maskedAddress;
                    }
                    yield break;
                }

                var newChar = ch == '1' ? '1' : addressBits[pos];
                working.Add(newChar);
                foreach (var maskedAddress in MaskAddressHelper(addressBits, working, pos + 1))
                {
                    yield return maskedAddress;
                }
            }
        }
    }
}
