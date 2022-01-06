// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR2.Serialization;

namespace fhirCsR2.Models
{
  /// <summary>
  /// This resource provides eligibility and plan details from the processing of an Eligibility resource.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<EligibilityResponse>))]
  public class EligibilityResponse : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "EligibilityResponse";
    /// <summary>
    /// The date when the enclosed suite of services were performed or completed.
    /// </summary>
    public string Created { get; set; }
    /// <summary>
    /// Extension container element for Created
    /// </summary>
    public Element _Created { get; set; }
    /// <summary>
    /// A description of the status of the adjudication.
    /// </summary>
    public string Disposition { get; set; }
    /// <summary>
    /// Extension container element for Disposition
    /// </summary>
    public Element _Disposition { get; set; }
    /// <summary>
    /// The Response business identifier.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The Insurer who produced this adjudicated response.
    /// </summary>
    public Reference Organization { get; set; }
    /// <summary>
    /// Knowledge of the original version can inform the processing of this instance so that information which is processable by the originating system may be generated.
    /// </summary>
    public Coding OriginalRuleset { get; set; }
    /// <summary>
    /// Transaction status: error, complete.
    /// </summary>
    public string Outcome { get; set; }
    /// <summary>
    /// Extension container element for Outcome
    /// </summary>
    public Element _Outcome { get; set; }
    /// <summary>
    /// Original request resource reference.
    /// </summary>
    public Reference Request { get; set; }
    /// <summary>
    /// The organization which is responsible for the services rendered to the patient.
    /// </summary>
    public Reference RequestOrganization { get; set; }
    /// <summary>
    /// The practitioner who is responsible for the services rendered to the patient.
    /// </summary>
    public Reference RequestProvider { get; set; }
    /// <summary>
    /// The version of the style of resource contents. This should be mapped to the allowable profiles for this and supporting resources.
    /// </summary>
    public Coding Ruleset { get; set; }
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


      ((fhirCsR2.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if (Request != null)
      {
        writer.WritePropertyName("request");
        Request.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Outcome))
      {
        writer.WriteString("outcome", (string)Outcome!);
      }

      if (_Outcome != null)
      {
        writer.WritePropertyName("_outcome");
        _Outcome.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Disposition))
      {
        writer.WriteString("disposition", (string)Disposition!);
      }

      if (_Disposition != null)
      {
        writer.WritePropertyName("_disposition");
        _Disposition.SerializeJson(writer, options);
      }

      if (Ruleset != null)
      {
        writer.WritePropertyName("ruleset");
        Ruleset.SerializeJson(writer, options);
      }

      if (OriginalRuleset != null)
      {
        writer.WritePropertyName("originalRuleset");
        OriginalRuleset.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Created))
      {
        writer.WriteString("created", (string)Created!);
      }

      if (_Created != null)
      {
        writer.WritePropertyName("_created");
        _Created.SerializeJson(writer, options);
      }

      if (Organization != null)
      {
        writer.WritePropertyName("organization");
        Organization.SerializeJson(writer, options);
      }

      if (RequestProvider != null)
      {
        writer.WritePropertyName("requestProvider");
        RequestProvider.SerializeJson(writer, options);
      }

      if (RequestOrganization != null)
      {
        writer.WritePropertyName("requestOrganization");
        RequestOrganization.SerializeJson(writer, options);
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
        case "created":
          Created = reader.GetString();
          break;

        case "_created":
          _Created = new fhirCsR2.Models.Element();
          _Created.DeserializeJson(ref reader, options);
          break;

        case "disposition":
          Disposition = reader.GetString();
          break;

        case "_disposition":
          _Disposition = new fhirCsR2.Models.Element();
          _Disposition.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Identifier objIdentifier = new fhirCsR2.Models.Identifier();
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

        case "organization":
          Organization = new fhirCsR2.Models.Reference();
          Organization.DeserializeJson(ref reader, options);
          break;

        case "originalRuleset":
          OriginalRuleset = new fhirCsR2.Models.Coding();
          OriginalRuleset.DeserializeJson(ref reader, options);
          break;

        case "outcome":
          Outcome = reader.GetString();
          break;

        case "_outcome":
          _Outcome = new fhirCsR2.Models.Element();
          _Outcome.DeserializeJson(ref reader, options);
          break;

        case "request":
          Request = new fhirCsR2.Models.Reference();
          Request.DeserializeJson(ref reader, options);
          break;

        case "requestOrganization":
          RequestOrganization = new fhirCsR2.Models.Reference();
          RequestOrganization.DeserializeJson(ref reader, options);
          break;

        case "requestProvider":
          RequestProvider = new fhirCsR2.Models.Reference();
          RequestProvider.DeserializeJson(ref reader, options);
          break;

        case "ruleset":
          Ruleset = new fhirCsR2.Models.Coding();
          Ruleset.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the EligibilityResponse.outcome field
  /// </summary>
  public static class EligibilityResponseOutcomeCodes {
    public const string COMPLETE = "complete";
    public const string ERROR = "error";
  }
}
