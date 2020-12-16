using Advent.Text.TravelDocuments;

namespace Advent.Text.Solvers
{
    [Solver("16.2")]
    public class Day16Part02 : Solver
    { 

        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var trainTicketData = TrainTicketData.Parse(rawInput);
            trainTicketData.IdentifyFields();

            return trainTicketData.GetDepartureSummary();
        }

    }
}
