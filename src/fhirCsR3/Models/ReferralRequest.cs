// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR3.Serialization;

namespace fhirCsR3.Models
{
  /// <summary>
  /// The individual who initiated the request and has responsibility for its activation.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ReferralRequestRequester>))]
  public class ReferralRequestRequester : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The device, practitioner, etc. who initiated the request.
    /// </summary>
    public Reference Agent { get; set; }
    /// <summary>
    /// The organization the device or practitioner was acting on behalf of.
    /// </summary>
    public Reference OnBehalfOf { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Agent != null)
      {
        writer.WritePropertyName("agent");
        Agent.SerializeJson(writer, options);
      }

      if (OnBehalfOf != null)
      {
        writer.WritePropertyName("onBehalfOf");
        OnBehalfOf.SerializeJson(writer, options);
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
        case "agent":
          Agent = new fhirCsR3.Models.Reference();
          Agent.DeserializeJson(ref reader, options);
          break;

        case "onBehalfOf":
          OnBehalfOf = new fhirCsR3.Models.Reference();
          OnBehalfOf.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Used to record and send details about a request for referral service or transfer of a patient to the care of another provider or provider organization.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ReferralRequest>))]
  public class ReferralRequest : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ReferralRequest";
    /// <summary>
    /// Date/DateTime of creation for draft requests and date of activation for active requests.
    /// </summary>
    public string AuthoredOn { get; set; }
    /// <summary>
    /// Extension container element for AuthoredOn
    /// </summary>
    public Element _AuthoredOn { get; set; }
    /// <summary>
    /// Indicates any plans, proposals or orders that this request is intended to satisfy - in whole or in part.
    /// </summary>
    public List<Reference> BasedOn { get; set; }
    /// <summary>
    /// The encounter at which the request for referral or transfer of care is initiated.
    /// </summary>
    public Reference Context { get; set; }
    /// <summary>
    /// A protocol, guideline, orderset or other definition that is adhered to in whole or in part by this request.
    /// </summary>
    public List<Reference> Definition { get; set; }
    /// <summary>
    /// The reason element gives a short description of why the referral is being made, the description expands on this to support a more complete clinical summary.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// The business identifier of the logical "grouping" request/order that this referral is a part of.
    /// </summary>
    public Identifier GroupIdentifier { get; set; }
    /// <summary>
    /// Business identifier that uniquely identifies the referral/care transfer request instance.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Distinguishes the "level" of authorization/demand implicit in this request.
    /// </summary>
    public string Intent { get; set; }
    /// <summary>
    /// Extension container element for Intent
    /// </summary>
    public Element _Intent { get; set; }
    /// <summary>
    /// Comments made about the referral request by any of the participants.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// When the occurrenceDateTime is used, then it is indicating that the requested service must happen before the specified date.
    /// </summary>
    public string OccurrenceDateTime { get; set; }
    /// <summary>
    /// Extension container element for OccurrenceDateTime
    /// </summary>
    public Element _OccurrenceDateTime { get; set; }
    /// <summary>
    /// When the occurrenceDateTime is used, then it is indicating that the requested service must happen before the specified date.
    /// </summary>
    public Period OccurrencePeriod { get; set; }
    /// <summary>
    /// An indication of the urgency of referral (or where applicable the type of transfer of care) request.
    /// </summary>
    public string Priority { get; set; }
    /// <summary>
    /// Extension container element for Priority
    /// </summary>
    public Element _Priority { get; set; }
    /// <summary>
    /// Description of clinical condition indicating why referral/transfer of care is requested.  For example:  Pathological Anomalies, Disabled (physical or mental),  Behavioral Management.
    /// </summary>
    public List<CodeableConcept> ReasonCode { get; set; }
    /// <summary>
    /// Indicates another resource whose existence justifies this request.
    /// </summary>
    public List<Reference> ReasonReference { get; set; }
    /// <summary>
    /// There will be a primary receiver. But the request can be received by any number of "copied to" providers or organizations.
    /// </summary>
    public List<Reference> Recipient { get; set; }
    /// <summary>
    /// This element does not point to the Provenance associated with the *current* version of the resource - as it would be created after this version existed.  The Provenance for the current version can be retrieved with a _revinclude.
    /// </summary>
    public List<Reference> RelevantHistory { get; set; }
    /// <summary>
    /// The replacement could be because the initial request was immediately rejected (due to an issue) or because the previous request was completed, but the need for the action described by the request remains ongoing.
    /// </summary>
    public List<Reference> Replaces { get; set; }
    /// <summary>
    /// The individual who initiated the request and has responsibility for its activation.
    /// </summary>
    public ReferralRequestRequester Requester { get; set; }
    /// <summary>
    /// The service(s) that is/are requested to be provided to the patient.  For example: cardiac pacemaker insertion.
    /// </summary>
    public List<CodeableConcept> ServiceRequested { get; set; }
    /// <summary>
    /// Indication of the clinical domain or discipline to which the referral or transfer of care request is sent.  For example: Cardiology Gastroenterology Diabetology.
    /// </summary>
    public CodeableConcept Specialty { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains codes that mark the resource as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Referral of family, group or community is to be catered for by profiles.
    /// </summary>
    public Reference Subject { get; set; }
    /// <summary>
    /// Any additional (administrative, financial or clinical) information required to support request for referral or transfer of care.  For example: Presenting problems/chief complaints Medical History Family History Alerts Allergy/Intolerance and Adverse Reactions Medications Observations/Assessments (may include cognitive and fundtional assessments) Diagnostic Reports Care Plan.
    /// </summary>
    public List<Reference> SupportingInfo { get; set; }
    /// <summary>
    /// An indication of the type of referral (or where applicable the type of transfer of care) request.
    /// </summary>
    public CodeableConcept Type { get; set; }
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


      ((fhirCsR3.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if ((Definition != null) && (Definition.Count != 0))
      {
        writer.WritePropertyName("definition");
        writer.WriteStartArray();

        foreach (Reference valDefinition in Definition)
        {
          valDefinition.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((BasedOn != null) && (BasedOn.Count != 0))
      {
        writer.WritePropertyName("basedOn");
        writer.WriteStartArray();

        foreach (Reference valBasedOn in BasedOn)
        {
          valBasedOn.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Replaces != null) && (Replaces.Count != 0))
      {
        writer.WritePropertyName("replaces");
        writer.WriteStartArray();

        foreach (Reference valReplaces in Replaces)
        {
          valReplaces.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (GroupIdentifier != null)
      {
        writer.WritePropertyName("groupIdentifier");
        GroupIdentifier.SerializeJson(writer, options);
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

      if (!string.IsNullOrEmpty(Intent))
      {
        writer.WriteString("intent", (string)Intent!);
      }

      if (_Intent != null)
      {
        writer.WritePropertyName("_intent");
        _Intent.SerializeJson(writer, options);
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Priority))
      {
        writer.WriteString("priority", (string)Priority!);
      }

      if (_Priority != null)
      {
        writer.WritePropertyName("_priority");
        _Priority.SerializeJson(writer, options);
      }

      if ((ServiceRequested != null) && (ServiceRequested.Count != 0))
      {
        writer.WritePropertyName("serviceRequested");
        writer.WriteStartArray();

        foreach (CodeableConcept valServiceRequested in ServiceRequested)
        {
          valServiceRequested.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Subject != null)
      {
        writer.WritePropertyName("subject");
        Subject.SerializeJson(writer, options);
      }

      if (Context != null)
      {
        writer.WritePropertyName("context");
        Context.SerializeJson(writer, options);
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

      if (!string.IsNullOrEmpty(AuthoredOn))
      {
        writer.WriteString("authoredOn", (string)AuthoredOn!);
      }

      if (_AuthoredOn != null)
      {
        writer.WritePropertyName("_authoredOn");
        _AuthoredOn.SerializeJson(writer, options);
      }

      if (Requester != null)
      {
        writer.WritePropertyName("requester");
        Requester.SerializeJson(writer, options);
      }

      if (Specialty != null)
      {
        writer.WritePropertyName("specialty");
        Specialty.SerializeJson(writer, options);
      }

      if ((Recipient != null) && (Recipient.Count != 0))
      {
        writer.WritePropertyName("recipient");
        writer.WriteStartArray();

        foreach (Reference valRecipient in Recipient)
        {
          valRecipient.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if ((SupportingInfo != null) && (SupportingInfo.Count != 0))
      {
        writer.WritePropertyName("supportingInfo");
        writer.WriteStartArray();

        foreach (Reference valSupportingInfo in SupportingInfo)
        {
          valSupportingInfo.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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

      if ((RelevantHistory != null) && (RelevantHistory.Count != 0))
      {
        writer.WritePropertyName("relevantHistory");
        writer.WriteStartArray();

        foreach (Reference valRelevantHistory in RelevantHistory)
        {
          valRelevantHistory.SerializeJson(writer, options, true);
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
        case "authoredOn":
          AuthoredOn = reader.GetString();
          break;

        case "_authoredOn":
          _AuthoredOn = new fhirCsR3.Models.Element();
          _AuthoredOn.DeserializeJson(ref reader, options);
          break;

        case "basedOn":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          BasedOn = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objBasedOn = new fhirCsR3.Models.Reference();
            objBasedOn.DeserializeJson(ref reader, options);
            BasedOn.Add(objBasedOn);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (BasedOn.Count == 0)
          {
            BasedOn = null;
          }

          break;

        case "context":
          Context = new fhirCsR3.Models.Reference();
          Context.DeserializeJson(ref reader, options);
          break;

        case "definition":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Definition = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objDefinition = new fhirCsR3.Models.Reference();
            objDefinition.DeserializeJson(ref reader, options);
            Definition.Add(objDefinition);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Definition.Count == 0)
          {
            Definition = null;
          }

          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR3.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "groupIdentifier":
          GroupIdentifier = new fhirCsR3.Models.Identifier();
          GroupIdentifier.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Identifier objIdentifier = new fhirCsR3.Models.Identifier();
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

        case "intent":
          Intent = reader.GetString();
          break;

        case "_intent":
          _Intent = new fhirCsR3.Models.Element();
          _Intent.DeserializeJson(ref reader, options);
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Annotation objNote = new fhirCsR3.Models.Annotation();
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
          _OccurrenceDateTime = new fhirCsR3.Models.Element();
          _OccurrenceDateTime.DeserializeJson(ref reader, options);
          break;

        case "occurrencePeriod":
          OccurrencePeriod = new fhirCsR3.Models.Period();
          OccurrencePeriod.DeserializeJson(ref reader, options);
          break;

        case "priority":
          Priority = reader.GetString();
          break;

        case "_priority":
          _Priority = new fhirCsR3.Models.Element();
          _Priority.DeserializeJson(ref reader, options);
          break;

        case "reasonCode":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ReasonCode = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objReasonCode = new fhirCsR3.Models.CodeableConcept();
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
            fhirCsR3.Models.Reference objReasonReference = new fhirCsR3.Models.Reference();
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

        case "recipient":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Recipient = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objRecipient = new fhirCsR3.Models.Reference();
            objRecipient.DeserializeJson(ref reader, options);
            Recipient.Add(objRecipient);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Recipient.Count == 0)
          {
            Recipient = null;
          }

          break;

        case "relevantHistory":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          RelevantHistory = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objRelevantHistory = new fhirCsR3.Models.Reference();
            objRelevantHistory.DeserializeJson(ref reader, options);
            RelevantHistory.Add(objRelevantHistory);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (RelevantHistory.Count == 0)
          {
            RelevantHistory = null;
          }

          break;

        case "replaces":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Replaces = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objReplaces = new fhirCsR3.Models.Reference();
            objReplaces.DeserializeJson(ref reader, options);
            Replaces.Add(objReplaces);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Replaces.Count == 0)
          {
            Replaces = null;
          }

          break;

        case "requester":
          Requester = new fhirCsR3.Models.ReferralRequestRequester();
          Requester.DeserializeJson(ref reader, options);
          break;

        case "serviceRequested":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ServiceRequested = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objServiceRequested = new fhirCsR3.Models.CodeableConcept();
            objServiceRequested.DeserializeJson(ref reader, options);
            ServiceRequested.Add(objServiceRequested);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ServiceRequested.Count == 0)
          {
            ServiceRequested = null;
          }

          break;

        case "specialty":
          Specialty = new fhirCsR3.Models.CodeableConcept();
          Specialty.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new fhirCsR3.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
          break;

        case "supportingInfo":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          SupportingInfo = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objSupportingInfo = new fhirCsR3.Models.Reference();
            objSupportingInfo.DeserializeJson(ref reader, options);
            SupportingInfo.Add(objSupportingInfo);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (SupportingInfo.Count == 0)
          {
            SupportingInfo = null;
          }

          break;

        case "type":
          Type = new fhirCsR3.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the ReferralRequest.intent field
  /// </summary>
  public static class ReferralRequestIntentCodes {
    public const string PROPOSAL = "proposal";
    public const string PLAN = "plan";
    public const string ORDER = "order";
    public const string ORIGINAL_ORDER = "original-order";
    public const string REFLEX_ORDER = "reflex-order";
    public const string FILLER_ORDER = "filler-order";
    public const string INSTANCE_ORDER = "instance-order";
    public const string OPTION = "option";
  }
  /// <summary>
  /// Code Values for the ReferralRequest.status field
  /// </summary>
  public static class ReferralRequestStatusCodes {
    public const string DRAFT = "draft";
    public const string ACTIVE = "active";
    public const string SUSPENDED = "suspended";
    public const string CANCELLED = "cancelled";
    public const string COMPLETED = "completed";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string UNKNOWN = "unknown";
  }
}
