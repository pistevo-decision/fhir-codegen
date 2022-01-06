// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Type if a sequence -- DNA, RNA, or amino acid sequence.
  /// </summary>
  public static class SequenceTypeCodes
  {
    /// <summary>
    /// Amino acid sequence.
    /// </summary>
    public static readonly Coding AASequence = new Coding
    {
      Code = "aa",
      Display = "AA Sequence",
      System = "http://hl7.org/fhir/sequence-type"
    };
    /// <summary>
    /// DNA Sequence.
    /// </summary>
    public static readonly Coding DNASequence = new Coding
    {
      Code = "dna",
      Display = "DNA Sequence",
      System = "http://hl7.org/fhir/sequence-type"
    };
    /// <summary>
    /// RNA Sequence.
    /// </summary>
    public static readonly Coding RNASequence = new Coding
    {
      Code = "rna",
      Display = "RNA Sequence",
      System = "http://hl7.org/fhir/sequence-type"
    };

    /// <summary>
    /// Literal for code: AASequence
    /// </summary>
    public const string LiteralAASequence = "aa";

    /// <summary>
    /// Literal for code: DNASequence
    /// </summary>
    public const string LiteralDNASequence = "dna";

    /// <summary>
    /// Literal for code: RNASequence
    /// </summary>
    public const string LiteralRNASequence = "rna";
  };
}
