// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// Multiple repetitions can be used to identify the same type of outcome in different timeframes as well as different types of outcomes.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<RiskAssessmentPrediction>))]
  public class RiskAssessmentPrediction : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// One of the potential outcomes for the patient (e.g. remission, death,  a particular condition).
    /// </summary>
    public CodeableConcept Outcome { get; set; }
    /// <summary>
    /// If range is used, it represents the lower and upper bounds of certainty; e.g. 40-60%  Decimal values are expressed as percentages as well (max = 100).
    /// </summary>
    public decimal? ProbabilityDecimal { get; set; }
    /// <summary>
    /// Extension container element for ProbabilityDecimal
    /// </summary>
    public Element _ProbabilityDecimal { get; set; }
    /// <summary>
    /// If range is used, it represents the lower and upper bounds of certainty; e.g. 40-60%  Decimal values are expressed as percentages as well (max = 100).
    /// </summary>
    public Range ProbabilityRange { get; set; }
    /// <summary>
    /// Indicates how likely the outcome is (in the specified timeframe), expressed as a qualitative value (e.g. low, medium, or high).
    /// </summary>
    public CodeableConcept QualitativeRisk { get; set; }
    /// <summary>
    /// Additional information explaining the basis for the prediction.
    /// </summary>
    public string Rationale { get; set; }
    /// <summary>
    /// Extension container element for Rationale
    /// </summary>
    public Element _Rationale { get; set; }
    /// <summary>
    /// Indicates the risk for this particular subject (with their specific characteristics) divided by the risk of the population in general.  (Numbers greater than 1 = higher risk than the population, numbers less than 1 = lower risk.).
    /// </summary>
    public decimal? RelativeRisk { get; set; }
    /// <summary>
    /// Extension container element for RelativeRisk
    /// </summary>
    public Element _RelativeRisk { get; set; }
    /// <summary>
    /// If not specified, the risk applies "over the subject's lifespan".
    /// </summary>
    public Period WhenPeriod { get; set; }
    /// <summary>
    /// If not specified, the risk applies "over the subject's lifespan".
    /// </summary>
    public Range WhenRange { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Outcome != null)
      {
        writer.WritePropertyName("outcome");
        Outcome.SerializeJson(writer, options);
      }

      if (ProbabilityDecimal != null)
      {
        writer.WriteNumber("probabilityDecimal", (decimal)ProbabilityDecimal!);
      }

      if (_ProbabilityDecimal != null)
      {
        writer.WritePropertyName("_probabilityDecimal");
        _ProbabilityDecimal.SerializeJson(writer, options);
      }

      if (ProbabilityRange != null)
      {
        writer.WritePropertyName("probabilityRange");
        ProbabilityRange.SerializeJson(writer, options);
      }

      if (QualitativeRisk != null)
      {
        writer.WritePropertyName("qualitativeRisk");
        QualitativeRisk.SerializeJson(writer, options);
      }

      if (RelativeRisk != null)
      {
        writer.WriteNumber("relativeRisk", (decimal)RelativeRisk!);
      }

      if (_RelativeRisk != null)
      {
        writer.WritePropertyName("_relativeRisk");
        _RelativeRisk.SerializeJson(writer, options);
      }

      if (WhenPeriod != null)
      {
        writer.WritePropertyName("whenPeriod");
        WhenPeriod.SerializeJson(writer, options);
      }

      if (WhenRange != null)
      {
        writer.WritePropertyName("whenRange");
        WhenRange.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Rationale))
      {
        writer.WriteString("rationale", (string)Rationale!);
      }

      if (_Rationale != null)
      {
        writer.WritePropertyName("_rationale");
        _Rationale.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "outcome":
          Outcome = new fhirCsR5.Models.CodeableConcept();
          Outcome.DeserializeJson(ref reader, options);
          break;

        case "probabilityDecimal":
          ProbabilityDecimal = reader.GetDecimal();
          break;

        case "_probabilityDecimal":
          _ProbabilityDecimal = new fhirCsR5.Models.Element();
          _ProbabilityDecimal.DeserializeJson(ref reader, options);
          break;

        case "probabilityRange":
          ProbabilityRange = new fhirCsR5.Models.Range();
          ProbabilityRange.DeserializeJson(ref reader, options);
          break;

        case "qualitativeRisk":
          QualitativeRisk = new fhirCsR5.Models.CodeableConcept();
          QualitativeRisk.DeserializeJson(ref reader, options);
          break;

        case "rationale":
          Rationale = reader.GetString();
          break;

        case "_rationale":
          _Rationale = new fhirCsR5.Models.Element();
          _Rationale.DeserializeJson(ref reader, options);
          break;

        case "relativeRisk":
          RelativeRisk = reader.GetDecimal();
          break;

        case "_relativeRisk":
          _RelativeRisk = new fhirCsR5.Models.Element();
          _RelativeRisk.DeserializeJson(ref reader, options);
          break;

        case "whenPeriod":
          WhenPeriod = new fhirCsR5.Models.Period();
          WhenPeriod.DeserializeJson(ref reader, options);
          break;

        case "whenRange":
          WhenRange = new fhirCsR5.Models.Range();
          WhenRange.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// An assessment of the likely outcome(s) for a patient or other subject as well as the likelihood of each outcome.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<RiskAssessment>))]
  public class RiskAssessment : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "RiskAssessment";
    /// <summary>
    /// A reference to the request that is fulfilled by this risk assessment.
    /// </summary>
    public Reference BasedOn { get; set; }
    /// <summary>
    /// Indicates the source data considered as part of the assessment (for example, FamilyHistory, Observations, Procedures, Conditions, etc.).
    /// </summary>
    public List<Reference> Basis { get; set; }
    /// <summary>
    /// The type of the risk assessment performed.
    /// </summary>
    public CodeableConcept Code { get; set; }
    /// <summary>
    /// For assessments or prognosis specific to a particular condition, indicates the condition being assessed.
    /// </summary>
    public Reference Condition { get; set; }
    /// <summary>
    /// The encounter where the assessment was performed.
    /// </summary>
    public Reference Encounter { get; set; }
    /// <summary>
    /// Business identifier assigned to the risk assessment.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The algorithm, process or mechanism used to evaluate the risk.
    /// </summary>
    public CodeableConcept Method { get; set; }
    /// <summary>
    /// A description of the steps that might be taken to reduce the identified risk(s).
    /// </summary>
    public string Mitigation { get; set; }
    /// <summary>
    /// Extension container element for Mitigation
    /// </summary>
    public Element _Mitigation { get; set; }
    /// <summary>
    /// Additional comments about the risk assessment.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// The date (and possibly time) the risk assessment was performed.
    /// </summary>
    public string OccurrenceDateTime { get; set; }
    /// <summary>
    /// Extension container element for OccurrenceDateTime
    /// </summary>
    public Element _OccurrenceDateTime { get; set; }
    /// <summary>
    /// The date (and possibly time) the risk assessment was performed.
    /// </summary>
    public Period OccurrencePeriod { get; set; }
    /// <summary>
    /// A reference to a resource that this risk assessment is part of, such as a Procedure.
    /// </summary>
    public Reference Parent { get; set; }
    /// <summary>
    /// The provider, patient, related person, or software application that performed the assessment.
    /// </summary>
    public Reference Performer { get; set; }
    /// <summary>
    /// Multiple repetitions can be used to identify the same type of outcome in different timeframes as well as different types of outcomes.
    /// </summary>
    public List<RiskAssessmentPrediction> Prediction { get; set; }
    /// <summary>
    /// The reason the risk assessment was performed.
    /// </summary>
    public List<CodeableReference> Reason { get; set; }
    /// <summary>
    /// The status of the RiskAssessment, using the same statuses as an Observation.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The patient or group the risk assessment applies to.
    /// </summary>
    public Reference Subject { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR5.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (BasedOn != null)
      {
        writer.WritePropertyName("basedOn");
        BasedOn.SerializeJson(writer, options);
      }

      if (Parent != null)
      {
        writer.WritePropertyName("parent");
        Parent.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (Method != null)
      {
        writer.WritePropertyName("method");
        Method.SerializeJson(writer, options);
      }

      if (Code != null)
      {
        writer.WritePropertyName("code");
        Code.SerializeJson(writer, options);
      }

      if (Subject != null)
      {
        writer.WritePropertyName("subject");
        Subject.SerializeJson(writer, options);
      }

      if (Encounter != null)
      {
        writer.WritePropertyName("encounter");
        Encounter.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(OccurrenceDateTime))
      {
        writer.WriteString("occurrenceDateTime", (string)OccurrenceDateTime!);
      }

      if (_OccurrenceDateTime != null)
      {
        writer.WritePropertyName("_occurrenceDateTime");
        _OccurrenceDateTime.SerializeJson(writer, options);
      }

      if (OccurrencePeriod != null)
      {
        writer.WritePropertyName("occurrencePeriod");
        OccurrencePeriod.SerializeJson(writer, options);
      }

      if (Condition != null)
      {
        writer.WritePropertyName("condition");
        Condition.SerializeJson(writer, options);
      }

      if (Performer != null)
      {
        writer.WritePropertyName("performer");
        Performer.SerializeJson(writer, options);
      }

      if ((Reason != null) && (Reason.Count != 0))
      {
        writer.WritePropertyName("reason");
        writer.WriteStartArray();

        foreach (CodeableReference valReason in Reason)
        {
          valReason.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Basis != null) && (Basis.Count != 0))
      {
        writer.WritePropertyName("basis");
        writer.WriteStartArray();

        foreach (Reference valBasis in Basis)
        {
          valBasis.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Prediction != null) && (Prediction.Count != 0))
      {
        writer.WritePropertyName("prediction");
        writer.WriteStartArray();

        foreach (RiskAssessmentPrediction valPrediction in Prediction)
        {
          valPrediction.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Mitigation))
      {
        writer.WriteString("mitigation", (string)Mitigation!);
      }

      if (_Mitigation != null)
      {
        writer.WritePropertyName("_mitigation");
        _Mitigation.SerializeJson(writer, options);
      }

      if ((Note != null) && (Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();

        foreach (Annotation valNote in Note)
        {
          valNote.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "basedOn":
          BasedOn = new fhirCsR5.Models.Reference();
          BasedOn.DeserializeJson(ref reader, options);
          break;

        case "basis":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Basis = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Reference objBasis = new fhirCsR5.Models.Reference();
            objBasis.DeserializeJson(ref reader, options);
            Basis.Add(objBasis);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Basis.Count == 0)
          {
            Basis = null;
          }

          break;

        case "code":
          Code = new fhirCsR5.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "condition":
          Condition = new fhirCsR5.Models.Reference();
          Condition.DeserializeJson(ref reader, options);
          break;

        case "encounter":
          Encounter = new fhirCsR5.Models.Reference();
          Encounter.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Identifier objIdentifier = new fhirCsR5.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "method":
          Method = new fhirCsR5.Models.CodeableConcept();
          Method.DeserializeJson(ref reader, options);
          break;

        case "mitigation":
          Mitigation = reader.GetString();
          break;

        case "_mitigation":
          _Mitigation = new fhirCsR5.Models.Element();
          _Mitigation.DeserializeJson(ref reader, options);
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Annotation objNote = new fhirCsR5.Models.Annotation();
            objNote.DeserializeJson(ref reader, options);
            Note.Add(objNote);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Note.Count == 0)
          {
            Note = null;
          }

          break;

        case "occurrenceDateTime":
          OccurrenceDateTime = reader.GetString();
          break;

        case "_occurrenceDateTime":
          _OccurrenceDateTime = new fhirCsR5.Models.Element();
          _OccurrenceDateTime.DeserializeJson(ref reader, options);
          break;

        case "occurrencePeriod":
          OccurrencePeriod = new fhirCsR5.Models.Period();
          OccurrencePeriod.DeserializeJson(ref reader, options);
          break;

        case "parent":
          Parent = new fhirCsR5.Models.Reference();
          Parent.DeserializeJson(ref reader, options);
          break;

        case "performer":
          Performer = new fhirCsR5.Models.Reference();
          Performer.DeserializeJson(ref reader, options);
          break;

        case "prediction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Prediction = new List<RiskAssessmentPrediction>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.RiskAssessmentPrediction objPrediction = new fhirCsR5.Models.RiskAssessmentPrediction();
            objPrediction.DeserializeJson(ref reader, options);
            Prediction.Add(objPrediction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Prediction.Count == 0)
          {
            Prediction = null;
          }

          break;

        case "reason":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Reason = new List<CodeableReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableReference objReason = new fhirCsR5.Models.CodeableReference();
            objReason.DeserializeJson(ref reader, options);
            Reason.Add(objReason);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Reason.Count == 0)
          {
            Reason = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR5.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new fhirCsR5.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the RiskAssessment.status field
  /// </summary>
  public static class RiskAssessmentStatusCodes {
    public const string REGISTERED = "registered";
    public const string PRELIMINARY = "preliminary";
    public const string FINAL = "final";
    public const string AMENDED = "amended";
    public const string CORRECTED = "corrected";
    public const string CANCELLED = "cancelled";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string UNKNOWN = "unknown";
  }
}
