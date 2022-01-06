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
  /// Text, attachment(s), or resource(s) that was communicated to the recipient.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<CommunicationPayload>))]
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
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

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
          _ContentString = new fhirCsR3.Models.Element();
          _ContentString.DeserializeJson(ref reader, options);
          break;

        case "contentAttachment":
          ContentAttachment = new fhirCsR3.Models.Attachment();
          ContentAttachment.DeserializeJson(ref reader, options);
          break;

        case "contentReference":
          ContentReference = new fhirCsR3.Models.Reference();
          ContentReference.DeserializeJson(ref reader, options);
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
  /// An occurrence of information being transmitted; e.g. an alert that was sent to a responsible provider, a public health agency was notified about a reportable condition.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<Communication>))]
  public class Communication : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Communication";
    /// <summary>
    /// This must point to some sort of a 'Request' resource, such as CarePlan, CommunicationRequest, ReferralRequest, MedicationRequest, etc.
    /// </summary>
    public List<Reference> BasedOn { get; set; }
    /// <summary>
    /// There may be multiple axes of categorization and one communication may serve multiple purposes.
    /// </summary>
    public List<CodeableConcept> Category { get; set; }
    /// <summary>
    /// The encounter within which the communication was sent.
    /// </summary>
    public Reference Context { get; set; }
    /// <summary>
    /// A protocol, guideline, or other definition that was adhered to in whole or in part by this communication event.
    /// </summary>
    public List<Reference> Definition { get; set; }
    /// <summary>
    /// Identifiers associated with this Communication that are defined by business processes and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// A channel that was used for this communication (e.g. email, fax).
    /// </summary>
    public List<CodeableConcept> Medium { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because it marks the communication as a communication that did not occur.  The more attributes are populated, the more constrained the negated statement is.
    /// </summary>
    public bool? NotDone { get; set; }
    /// <summary>
    /// Describes why the communication event did not occur in coded and/or textual form.
    /// </summary>
    public CodeableConcept NotDoneReason { get; set; }
    /// <summary>
    /// Additional notes or commentary about the communication by the sender, receiver or other interested parties.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// Part of this action.
    /// </summary>
    public List<Reference> PartOf { get; set; }
    /// <summary>
    /// Text, attachment(s), or resource(s) that was communicated to the recipient.
    /// </summary>
    public List<CommunicationPayload> Payload { get; set; }
    /// <summary>
    /// Textual reasons can be caprued using reasonCode.text.
    /// </summary>
    public List<CodeableConcept> ReasonCode { get; set; }
    /// <summary>
    /// Indicates another resource whose existence justifies this communication.
    /// </summary>
    public List<Reference> ReasonReference { get; set; }
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
    /// This element is labeled as a modifier because the status contains the codes aborted and entered-in-error that mark the communication as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// The patient or group that was the focus of this communication.
    /// </summary>
    public Reference Subject { get; set; }
    /// <summary>
    /// The resources which were responsible for or related to producing this communication.
    /// </summary>
    public List<Reference> Topic { get; set; }
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

      if ((PartOf != null) && (PartOf.Count != 0))
      {
        writer.WritePropertyName("partOf");
        writer.WriteStartArray();

        foreach (Reference valPartOf in PartOf)
        {
          valPartOf.SerializeJson(writer, options, true);
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

      if (NotDone != null)
      {
        writer.WriteBoolean("notDone", (bool)NotDone!);
      }

      if (NotDoneReason != null)
      {
        writer.WritePropertyName("notDoneReason");
        NotDoneReason.SerializeJson(writer, options);
      }

      if ((Category != null) && (Category.Count != 0))
      {
        writer.WritePropertyName("category");
        writer.WriteStartArray();

        foreach (CodeableConcept valCategory in Category)
        {
          valCategory.SerializeJson(writer, options, true);
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

      if (Subject != null)
      {
        writer.WritePropertyName("subject");
        Subject.SerializeJson(writer, options);
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

      if ((Topic != null) && (Topic.Count != 0))
      {
        writer.WritePropertyName("topic");
        writer.WriteStartArray();

        foreach (Reference valTopic in Topic)
        {
          valTopic.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Context != null)
      {
        writer.WritePropertyName("context");
        Context.SerializeJson(writer, options);
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

      if (Sender != null)
      {
        writer.WritePropertyName("sender");
        Sender.SerializeJson(writer, options);
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

        case "category":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Category = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objCategory = new fhirCsR3.Models.CodeableConcept();
            objCategory.DeserializeJson(ref reader, options);
            Category.Add(objCategory);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Category.Count == 0)
          {
            Category = null;
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

        case "medium":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Medium = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objMedium = new fhirCsR3.Models.CodeableConcept();
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

        case "notDone":
          NotDone = reader.GetBoolean();
          break;

        case "notDoneReason":
          NotDoneReason = new fhirCsR3.Models.CodeableConcept();
          NotDoneReason.DeserializeJson(ref reader, options);
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

        case "partOf":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          PartOf = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objPartOf = new fhirCsR3.Models.Reference();
            objPartOf.DeserializeJson(ref reader, options);
            PartOf.Add(objPartOf);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (PartOf.Count == 0)
          {
            PartOf = null;
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
            fhirCsR3.Models.CommunicationPayload objPayload = new fhirCsR3.Models.CommunicationPayload();
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

        case "received":
          Received = reader.GetString();
          break;

        case "_received":
          _Received = new fhirCsR3.Models.Element();
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

        case "sender":
          Sender = new fhirCsR3.Models.Reference();
          Sender.DeserializeJson(ref reader, options);
          break;

        case "sent":
          Sent = reader.GetString();
          break;

        case "_sent":
          _Sent = new fhirCsR3.Models.Element();
          _Sent.DeserializeJson(ref reader, options);
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

        case "topic":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Topic = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objTopic = new fhirCsR3.Models.Reference();
            objTopic.DeserializeJson(ref reader, options);
            Topic.Add(objTopic);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Topic.Count == 0)
          {
            Topic = null;
          }

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
  /// Code Values for the Communication.status field
  /// </summary>
  public static class CommunicationStatusCodes {
    public const string PREPARATION = "preparation";
    public const string IN_PROGRESS = "in-progress";
    public const string SUSPENDED = "suspended";
    public const string ABORTED = "aborted";
    public const string COMPLETED = "completed";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string UNKNOWN = "unknown";
  }
}
