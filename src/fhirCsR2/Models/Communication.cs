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
  /// Text, attachment(s), or resource(s) that was communicated to the recipient.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<CommunicationPayload>))]
  public class CommunicationPayload : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// A communicated content (or for multi-part communications, one portion of the communication).
    /// </summary>
    public string ContentString { get; set; }
    /// <summary>
    /// Extension container element for ContentString
    /// </summary>
    public Element _ContentString { get; set; }
    /// <summary>
    /// A communicated content (or for multi-part communications, one portion of the communication).
    /// </summary>
    public Attachment ContentAttachment { get; set; }
    /// <summary>
    /// A communicated content (or for multi-part communications, one portion of the communication).
    /// </summary>
    public Reference ContentReference { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR2.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(ContentString))
      {
        writer.WriteString("contentString", (string)ContentString!);
      }

      if (_ContentString != null)
      {
        writer.WritePropertyName("_contentString");
        _ContentString.SerializeJson(writer, options);
      }

      if (ContentAttachment != null)
      {
        writer.WritePropertyName("contentAttachment");
        ContentAttachment.SerializeJson(writer, options);
      }

      if (ContentReference != null)
      {
        writer.WritePropertyName("contentReference");
        ContentReference.SerializeJson(writer, options);
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
        case "contentString":
          ContentString = reader.GetString();
          break;

        case "_contentString":
          _ContentString = new fhirCsR2.Models.Element();
          _ContentString.DeserializeJson(ref reader, options);
          break;

        case "contentAttachment":
          ContentAttachment = new fhirCsR2.Models.Attachment();
          ContentAttachment.DeserializeJson(ref reader, options);
          break;

        case "contentReference":
          ContentReference = new fhirCsR2.Models.Reference();
          ContentReference.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR2.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// An occurrence of information being transmitted; e.g. an alert that was sent to a responsible provider, a public health agency was notified about a reportable condition.
  /// </summary>
  [JsonConverter(typeof(fhirCsR2.Serialization.JsonStreamComponentConverter<Communication>))]
  public class Communication : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Communication";
    /// <summary>
    /// The type of message conveyed such as alert, notification, reminder, instruction, etc.
    /// </summary>
    public CodeableConcept Category { get; set; }
    /// <summary>
    /// The encounter within which the communication was sent.
    /// </summary>
    public Reference Encounter { get; set; }
    /// <summary>
    /// Identifiers associated with this Communication that are defined by business processes and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// A channel that was used for this communication (e.g. email, fax).
    /// </summary>
    public List<CodeableConcept> Medium { get; set; }
    /// <summary>
    /// Text, attachment(s), or resource(s) that was communicated to the recipient.
    /// </summary>
    public List<CommunicationPayload> Payload { get; set; }
    /// <summary>
    /// The reason or justification for the communication.
    /// </summary>
    public List<CodeableConcept> Reason { get; set; }
    /// <summary>
    /// The time when this communication arrived at the destination.
    /// </summary>
    public string Received { get; set; }
    /// <summary>
    /// Extension container element for Received
    /// </summary>
    public Element _Received { get; set; }
    /// <summary>
    /// The entity (e.g. person, organization, clinical information system, or device) which was the target of the communication. If receipts need to be tracked by individual, a separate resource instance will need to be created for each recipient.  Multiple recipient communications are intended where either a receipt(s) is not tracked (e.g. a mass mail-out) or is captured in aggregate (all emails confirmed received by a particular time).
    /// </summary>
    public List<Reference> Recipient { get; set; }
    /// <summary>
    /// The communication request that was responsible for producing this communication.
    /// </summary>
    public Reference RequestDetail { get; set; }
    /// <summary>
    /// The entity (e.g. person, organization, clinical information system, or device) which was the source of the communication.
    /// </summary>
    public Reference Sender { get; set; }
    /// <summary>
    /// The time when this communication was sent.
    /// </summary>
    public string Sent { get; set; }
    /// <summary>
    /// Extension container element for Sent
    /// </summary>
    public Element _Sent { get; set; }
    /// <summary>
    /// The status of the transmission.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The patient who was the focus of this communication.
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

      if (Category != null)
      {
        writer.WritePropertyName("category");
        Category.SerializeJson(writer, options);
      }

      if (Sender != null)
      {
        writer.WritePropertyName("sender");
        Sender.SerializeJson(writer, options);
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

      if ((Payload != null) && (Payload.Count != 0))
      {
        writer.WritePropertyName("payload");
        writer.WriteStartArray();

        foreach (CommunicationPayload valPayload in Payload)
        {
          valPayload.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Medium != null) && (Medium.Count != 0))
      {
        writer.WritePropertyName("medium");
        writer.WriteStartArray();

        foreach (CodeableConcept valMedium in Medium)
        {
          valMedium.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
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

      if (Encounter != null)
      {
        writer.WritePropertyName("encounter");
        Encounter.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Sent))
      {
        writer.WriteString("sent", (string)Sent!);
      }

      if (_Sent != null)
      {
        writer.WritePropertyName("_sent");
        _Sent.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Received))
      {
        writer.WriteString("received", (string)Received!);
      }

      if (_Received != null)
      {
        writer.WritePropertyName("_received");
        _Received.SerializeJson(writer, options);
      }

      if ((Reason != null) && (Reason.Count != 0))
      {
        writer.WritePropertyName("reason");
        writer.WriteStartArray();

        foreach (CodeableConcept valReason in Reason)
        {
          valReason.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Subject != null)
      {
        writer.WritePropertyName("subject");
        Subject.SerializeJson(writer, options);
      }

      if (RequestDetail != null)
      {
        writer.WritePropertyName("requestDetail");
        RequestDetail.SerializeJson(writer, options);
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
        case "category":
          Category = new fhirCsR2.Models.CodeableConcept();
          Category.DeserializeJson(ref reader, options);
          break;

        case "encounter":
          Encounter = new fhirCsR2.Models.Reference();
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

        case "medium":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Medium = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CodeableConcept objMedium = new fhirCsR2.Models.CodeableConcept();
            objMedium.DeserializeJson(ref reader, options);
            Medium.Add(objMedium);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Medium.Count == 0)
          {
            Medium = null;
          }

          break;

        case "payload":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Payload = new List<CommunicationPayload>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CommunicationPayload objPayload = new fhirCsR2.Models.CommunicationPayload();
            objPayload.DeserializeJson(ref reader, options);
            Payload.Add(objPayload);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Payload.Count == 0)
          {
            Payload = null;
          }

          break;

        case "reason":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Reason = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.CodeableConcept objReason = new fhirCsR2.Models.CodeableConcept();
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

        case "received":
          Received = reader.GetString();
          break;

        case "_received":
          _Received = new fhirCsR2.Models.Element();
          _Received.DeserializeJson(ref reader, options);
          break;

        case "recipient":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Recipient = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR2.Models.Reference objRecipient = new fhirCsR2.Models.Reference();
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

        case "requestDetail":
          RequestDetail = new fhirCsR2.Models.Reference();
          RequestDetail.DeserializeJson(ref reader, options);
          break;

        case "sender":
          Sender = new fhirCsR2.Models.Reference();
          Sender.DeserializeJson(ref reader, options);
          break;

        case "sent":
          Sent = reader.GetString();
          break;

        case "_sent":
          _Sent = new fhirCsR2.Models.Element();
          _Sent.DeserializeJson(ref reader, options);
          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR2.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "subject":
          Subject = new fhirCsR2.Models.Reference();
          Subject.DeserializeJson(ref reader, options);
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
  /// Code Values for the Communication.status field
  /// </summary>
  public static class CommunicationStatusCodes {
    public const string IN_PROGRESS = "in-progress";
    public const string COMPLETED = "completed";
    public const string SUSPENDED = "suspended";
    public const string REJECTED = "rejected";
    public const string FAILED = "failed";
  }
}
