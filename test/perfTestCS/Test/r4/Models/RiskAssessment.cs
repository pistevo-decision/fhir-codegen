// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using Fhir.R4.Serialization;

namespace Fhir.R4.Models
{
  /// <summary>
  /// Multiple repetitions can be used to identify the same type of outcome in different timeframes as well as different types of outcomes.
  /// </summary>
  [JsonConverter(typeof(Fhir.R4.Serialization.JsonStreamComponentConverter<RiskAssessmentPrediction>))]
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

      ((Fhir.R4.Models.BackboneElement)this).SerializeJson(writer, options, false);

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
          Outcome = new Fhir.R4.Models.CodeableConcept();
          Outcome.DeserializeJson(ref reader, options);
          break;

        case "probabilityDecimal":
          ProbabilityDecimal = reader.GetDecimal();
          break;

        case "_probabilityDecimal":
          _ProbabilityDecimal = new Fhir.R4.Models.Element();
          _ProbabilityDecimal.DeserializeJson(ref reader, options);
          break;

        case "probabilityRange":
          ProbabilityRange = new Fhir.R4.Models.Range();
          ProbabilityRange.DeserializeJson(ref reader, options);
          break;

        case "qualitativeRisk":
          QualitativeRisk = new Fhir.R4.Models.CodeableConcept();
          QualitativeRisk.DeserializeJson(ref reader, options);
          break;

        case "rationale":
          Rationale = reader.GetString();
          break;

        case "_rationale":
          _Rationale = new Fhir.R4.Models.Element();
          _Rationale.DeserializeJson(ref reader, options);
          break;

        case "relativeRisk":
          RelativeRisk = reader.GetDecimal();
          break;

        case "_relativeRisk":
          _RelativeRisk = new Fhir.R4.Models.Element();
          _RelativeRisk.DeserializeJson(ref reader, options);
          break;

        case "whenPeriod":
          WhenPeriod = new Fhir.R4.Models.Period();
          WhenPeriod.DeserializeJson(ref reader, options);
          break;

        case "whenRange":
          WhenRange = new Fhir.R4.Models.Range();
          WhenRange.DeserializeJson(ref reader, options);
          break;

        default:
          ((Fhir.R4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  [JsonConverter(typeof(Fhir.R4.Serialization.JsonStreamComponentConverter<RiskAssessment>))]
  public class RiskAssessment : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public string ResourceType => "RiskAssessment";
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
    /// The provider or software application that performed the assessment.
    /// </summary>
    public Reference Performer { get; set; }
    /// <summary>
    /// Multiple repetitions can be used to identify the same type of outcome in different timeframes as well as different types of outcomes.
    /// </summary>
    public List<RiskAssessmentPrediction> Prediction { get; set; }
    /// <summary>
    /// The reason the risk assessment was performed.
    /// </summary>
    public List<CodeableConcept> ReasonCode { get; set; }
    /// <summary>
    /// Resources supporting the reason the risk assessment was performed.
    /// </summary>
    public List<Reference> ReasonReference { get; set; }
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


      ((Fhir.R4.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      writer.WritePropertyName("subject");
      Subject.SerializeJson(writer, options);

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

      if ((ReasonCode != null) && (ReasonCode.Count != 0))
      {
        writer.WritePropertyName("reasonCode");
        writer.WriteStartArray();

        foreach (CodeableConcept valReasonCode in ReasonCode)
        {
          valReasonCode.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((ReasonReference != null) && (ReasonReference.Count != 0))
      {
        writer.WritePropertyName("reasonReference");
        writer.WriteStartArray();

        foreach (Reference valReasonReference in ReasonReference)
        {
          valReasonReference.SerializeJson(writer, options, true);
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
          BasedOn = new Fhir.R4.Models.Reference();
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
            Fhir.R4.Models.Reference objBasis = new Fhir.R4.Models.Reference();
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
          Code = new Fhir.R4.Models.CodeableConcept();
          Code.DeserializeJson(ref reader, options);
          break;

        case "condition":
          Condition = new Fhir.R4.Models.Reference();
          Condition.DeserializeJson(ref reader, options);
          break;

        case "encounter":
          Encounter = new Fhir.R4.Models.Reference();
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
            Fhir.R4.Models.Identifier objIdentifier = new Fhir.R4.Models.Identifier();
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
          Method = new Fhir.R4.Models.CodeableConcept();
          Method.DeserializeJson(ref reader, options);
          break;

        case "mitigation":
          Mitigation = reader.GetString();
          break;

        case "_mitigation":
          _Mitigation = new Fhir.R4.Models.Element();
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
            Fhir.R4.Models.Annotation objNote = new Fhir.R4.Models.Annotation();
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
          _OccurrenceDateTime = new Fhir.R4.Models.Element();
          _OccurrenceDateTime.DeserializeJson(ref reader, options);
          break;

        case "occurrencePeriod":
          OccurrencePeriod = new Fhir.R4.Models.Period();
          OccurrencePeriod.DeserializeJson(ref reader, options);
          break;

        case "parent":
          Parent = new Fhir.R4.Models.Reference();
          Parent.DeserializeJson(ref reader, options);
          break;

        case "performer":
          Performer = new Fhir.R4.Models.Reference();
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
            Fhir.R4.Models.RiskAssessmentPrediction objPrediction = new Fhir.R4.Models.RiskAssessmentPrediction();
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

        case "reasonCode":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ReasonCode = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.CodeableConcept objReasonCode = new Fhir.R4.Models.CodeableConcept();
            objReasonCode.DeserializeJson(ref reader, options);
            ReasonCode.Add(objReasonCode);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ReasonCode.Count == 0)
          {
            ReasonCode = null;
          }

          break;

        case "reasonReference":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ReasonReference = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Fhir.R4.Models.Reference objReasonReference = new Fhir.R4.Models.Reference();
            objReasonReference.DeserializeJson(ref reader, options);
            ReasonReference.Add(objReasonReference);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ReasonReference.Count == 0)
          {
            ReasonReference = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new Fhir.R4.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new Fhir.R4.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
          break;

        default:
          ((Fhir.R4.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
}
