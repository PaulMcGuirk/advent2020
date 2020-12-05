using System;
using System.Collections.Generic;

namespace Advent.Text.TravelDocuments
{
    /// <summary>
    /// A boarding pass on a plane.
    /// </summary>
    // The boarding pass code (i.e. FBFBBFFRLR) is a binary
    // encoding of the ID where the first 7 bits use (F, B) for (0, 1)
    // and the last 3 bits use (L, R) for (0, 1). With the same
    // encoding, the first 7 bits also capture the row number and the
    // last 3 bits capture the column
    public class BoardingPass
    {
        public int Row { get; }
        public int Col { get; }
        public int Id => 8 * Row + Col;
        public string Code { get; }

        public BoardingPass(string code)
        {
            Code = code;
            (Row, Col) = Decode(Code);
        }

        /// <summary>
        /// Decode a boarding pass code into a row and column
        /// </summary>
        /// <param name="code">The boarding pass code to code (ex: "FBFBBFFRLR")
        /// </param>
        /// <returns>The row and column, as a pair</returns>
        public static (int row, int col) Decode(string code)
        {
            var rowCode = code[..7];
            var colCode = code[7..];

            var row = BinaryDecode(rowCode, 'B', 'F');
            var col = BinaryDecode(colCode, 'R', 'L');

            return (row, col);
        }

        /// <summary>
        /// Given a string representing a number in binary, returns the
        /// correspnding number.
        /// </summary>
        /// <param name="code">The string to decode</param>
        /// <param name="one">The character representing 1</param>
        /// <param name="zero">The character representing 0</param>
        /// <returns>The decoded number</returns>
        private static int BinaryDecode(string code, char one, char zero)
            => Convert.ToInt32(code.Replace(one, '1').Replace(zero, '0'), 2);
    }
}
