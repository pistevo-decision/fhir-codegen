// <auto-generated/>
// Contents of: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Hl7.Fhir.Model;
using Hl7.Fhir.Model.JsonExtensions;
using Hl7.Fhir.Serialization;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model.JsonExtensions
{
  /// <summary>
  /// JSON Serialization Extensions for MedicationAdministration
  /// </summary>
  public static class MedicationAdministrationJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR MedicationAdministration into JSON
    /// </summary>
    public static void SerializeJson(this MedicationAdministration current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","MedicationAdministration");
      // Complex: MedicationAdministration, Export: MedicationAdministration, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if ((current.Identifier != null) && (current.Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();
        foreach (Identifier val in current.Identifier)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.InstantiatesElement != null) && (current.InstantiatesElement.Count != 0))
      {
        writer.WritePropertyName("instantiates");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (FhirUri val in current.InstantiatesElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (FhirUri val in current.InstantiatesElement)
        {
          if (string.IsNullOrEmpty(val.Value))
          {
            if (foundExtensions) { writer.WriteNullValue(); }
          }
          else
          {
            writer.WriteStringValue(val.Value);
          }

        }
        if (foundExtensions)
        {
          writer.WriteEndArray();
          writer.WritePropertyName("_instantiates");
          writer.WriteStartArray();
          foreach (FhirUri val in current.InstantiatesElement)
          {
            if (val.HasExtensions() || (!string.IsNullOrEmpty(val.ElementId)))
            {
              JsonStreamUtilities.SerializeExtensionList(writer,options,string.Empty,true,val.Extension,val.ElementId);
            }
            else
            {
              writer.WriteNullValue();
            }

          }
        }
        writer.WriteEndArray();
      }

      if ((current.PartOf != null) && (current.PartOf.Count != 0))
      {
        writer.WritePropertyName("partOf");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.PartOf)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      if ((current.StatusReason != null) && (current.StatusReason.Count != 0))
      {
        writer.WritePropertyName("statusReason");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.StatusReason)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Category != null)
      {
        writer.WritePropertyName("category");
        current.Category.SerializeJson(writer, options);
      }

      if (current.Medication != null)
      {
        switch (current.Medication)
        {
          case CodeableConcept v_CodeableConcept:
            writer.WritePropertyName("medicationCodeableConcept");
            v_CodeableConcept.SerializeJson(writer, options);
            break;
          case ResourceReference v_ResourceReference:
            writer.WritePropertyName("medicationReference");
            v_ResourceReference.SerializeJson(writer, options);
            break;
        }
      }
      writer.WritePropertyName("subject");
      current.Subject.SerializeJson(writer, options);

      if (current.Context != null)
      {
        writer.WritePropertyName("context");
        current.Context.SerializeJson(writer, options);
      }

      if ((current.SupportingInformation != null) && (current.SupportingInformation.Count != 0))
      {
        writer.WritePropertyName("supportingInformation");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.SupportingInformation)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Effective != null)
      {
        switch (current.Effective)
        {
          case FhirDateTime v_FhirDateTime:
            writer.WriteString("effectiveDateTime",v_FhirDateTime.Value);
            break;
          case Period v_Period:
            writer.WritePropertyName("effectivePeriod");
            v_Period.SerializeJson(writer, options);
            break;
        }
      }
      if ((current.Performer != null) && (current.Performer.Count != 0))
      {
        writer.WritePropertyName("performer");
        writer.WriteStartArray();
        foreach (MedicationAdministration.PerformerComponent val in current.Performer)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.ReasonCode != null) && (current.ReasonCode.Count != 0))
      {
        writer.WritePropertyName("reasonCode");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.ReasonCode)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.ReasonReference != null) && (current.ReasonReference.Count != 0))
      {
        writer.WritePropertyName("reasonReference");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.ReasonReference)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Request != null)
      {
        writer.WritePropertyName("request");
        current.Request.SerializeJson(writer, options);
      }

      if ((current.Device != null) && (current.Device.Count != 0))
      {
        writer.WritePropertyName("device");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Device)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Note != null) && (current.Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();
        foreach (Annotation val in current.Note)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Dosage != null)
      {
        writer.WritePropertyName("dosage");
        current.Dosage.SerializeJson(writer, options);
      }

      if ((current.EventHistory != null) && (current.EventHistory.Count != 0))
      {
        writer.WritePropertyName("eventHistory");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.EventHistory)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationAdministration
    /// </summary>
    public static void DeserializeJson(this MedicationAdministration current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationAdministration
    /// </summary>
    public static void DeserializeJsonProperty(this MedicationAdministration current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "instantiates":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.InstantiatesElement = new List<FhirUri>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.InstantiatesElement.Add(new FhirUri(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.InstantiatesElement.Count == 0)
          {
            current.InstantiatesElement = null;
          }
          break;

        case "_instantiates":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_instantiates = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_instantiates >= current.InstantiatesElement.Count)
            {
              current.InstantiatesElement.Add(new FhirUri());
            }
            ((Hl7.Fhir.Model.Element)current.InstantiatesElement[i_instantiates++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "partOf":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.PartOf = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_PartOf = new Hl7.Fhir.Model.ResourceReference();
            v_PartOf.DeserializeJson(ref reader, options);
            current.PartOf.Add(v_PartOf);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.PartOf.Count == 0)
          {
            current.PartOf = null;
          }
          break;

        case "status":
          current.StatusElement =new Code<Hl7.Fhir.Model.MedicationAdministration.MedicationAdministrationStatusCodes>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.MedicationAdministration.MedicationAdministrationStatusCodes>(reader.GetString()));
          break;

        case "_status":
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "statusReason":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.StatusReason = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_StatusReason = new Hl7.Fhir.Model.CodeableConcept();
            v_StatusReason.DeserializeJson(ref reader, options);
            current.StatusReason.Add(v_StatusReason);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.StatusReason.Count == 0)
          {
            current.StatusReason = null;
          }
          break;

        case "category":
          current.Category = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Category).DeserializeJson(ref reader, options);
          break;

        case "medicationCodeableConcept":
          current.Medication = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Medication).DeserializeJson(ref reader, options);
          break;

        case "medicationReference":
          current.Medication = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Medication).DeserializeJson(ref reader, options);
          break;

        case "subject":
          current.Subject = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Subject).DeserializeJson(ref reader, options);
          break;

        case "context":
          current.Context = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Context).DeserializeJson(ref reader, options);
          break;

        case "supportingInformation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.SupportingInformation = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_SupportingInformation = new Hl7.Fhir.Model.ResourceReference();
            v_SupportingInformation.DeserializeJson(ref reader, options);
            current.SupportingInformation.Add(v_SupportingInformation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.SupportingInformation.Count == 0)
          {
            current.SupportingInformation = null;
          }
          break;

        case "effectiveDateTime":
          current.Effective = new FhirDateTime(reader.GetString());
          break;

        case "effectivePeriod":
          current.Effective = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Effective).DeserializeJson(ref reader, options);
          break;

        case "performer":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Performer = new List<MedicationAdministration.PerformerComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.MedicationAdministration.PerformerComponent v_Performer = new Hl7.Fhir.Model.MedicationAdministration.PerformerComponent();
            v_Performer.DeserializeJson(ref reader, options);
            current.Performer.Add(v_Performer);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Performer.Count == 0)
          {
            current.Performer = null;
          }
          break;

        case "reasonCode":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.ReasonCode = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_ReasonCode = new Hl7.Fhir.Model.CodeableConcept();
            v_ReasonCode.DeserializeJson(ref reader, options);
            current.ReasonCode.Add(v_ReasonCode);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.ReasonCode.Count == 0)
          {
            current.ReasonCode = null;
          }
          break;

        case "reasonReference":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.ReasonReference = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_ReasonReference = new Hl7.Fhir.Model.ResourceReference();
            v_ReasonReference.DeserializeJson(ref reader, options);
            current.ReasonReference.Add(v_ReasonReference);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.ReasonReference.Count == 0)
          {
            current.ReasonReference = null;
          }
          break;

        case "request":
          current.Request = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Request).DeserializeJson(ref reader, options);
          break;

        case "device":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Device = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Device = new Hl7.Fhir.Model.ResourceReference();
            v_Device.DeserializeJson(ref reader, options);
            current.Device.Add(v_Device);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Device.Count == 0)
          {
            current.Device = null;
          }
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Annotation v_Note = new Hl7.Fhir.Model.Annotation();
            v_Note.DeserializeJson(ref reader, options);
            current.Note.Add(v_Note);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Note.Count == 0)
          {
            current.Note = null;
          }
          break;

        case "dosage":
          current.Dosage = new Hl7.Fhir.Model.MedicationAdministration.DosageComponent();
          ((Hl7.Fhir.Model.MedicationAdministration.DosageComponent)current.Dosage).DeserializeJson(ref reader, options);
          break;

        case "eventHistory":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.EventHistory = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_EventHistory = new Hl7.Fhir.Model.ResourceReference();
            v_EventHistory.DeserializeJson(ref reader, options);
            current.EventHistory.Add(v_EventHistory);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.EventHistory.Count == 0)
          {
            current.EventHistory = null;
          }
          break;

        // Complex: MedicationAdministration, Export: MedicationAdministration, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR MedicationAdministration#Performer into JSON
    /// </summary>
    public static void SerializeJson(this MedicationAdministration.PerformerComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: MedicationAdministration#Performer, Export: PerformerComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Function != null)
      {
        writer.WritePropertyName("function");
        current.Function.SerializeJson(writer, options);
      }

      writer.WritePropertyName("actor");
      current.Actor.SerializeJson(writer, options);

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationAdministration#Performer
    /// </summary>
    public static void DeserializeJson(this MedicationAdministration.PerformerComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationAdministration#Performer
    /// </summary>
    public static void DeserializeJsonProperty(this MedicationAdministration.PerformerComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "function":
          current.Function = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Function).DeserializeJson(ref reader, options);
          break;

        case "actor":
          current.Actor = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Actor).DeserializeJson(ref reader, options);
          break;

        // Complex: performer, Export: PerformerComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR MedicationAdministration#Dosage into JSON
    /// </summary>
    public static void SerializeJson(this MedicationAdministration.DosageComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: MedicationAdministration#Dosage, Export: DosageComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.TextElement != null)
      {
        if (!string.IsNullOrEmpty(current.TextElement.Value))
        {
          writer.WriteString("text",current.TextElement.Value);
        }
        if (current.TextElement.HasExtensions() || (!string.IsNullOrEmpty(current.TextElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_text",false,current.TextElement.Extension,current.TextElement.ElementId);
        }
      }

      if (current.Site != null)
      {
        writer.WritePropertyName("site");
        current.Site.SerializeJson(writer, options);
      }

      if (current.Route != null)
      {
        writer.WritePropertyName("route");
        current.Route.SerializeJson(writer, options);
      }

      if (current.Method != null)
      {
        writer.WritePropertyName("method");
        current.Method.SerializeJson(writer, options);
      }

      if (current.Dose != null)
      {
        writer.WritePropertyName("dose");
        current.Dose.SerializeJson(writer, options);
      }

      if (current.Rate != null)
      {
        switch (current.Rate)
        {
          case Ratio v_Ratio:
            writer.WritePropertyName("rateRatio");
            v_Ratio.SerializeJson(writer, options);
            break;
          case Quantity v_Quantity:
            writer.WritePropertyName("rateQuantity");
            v_Quantity.SerializeJson(writer, options);
            break;
        }
      }
      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationAdministration#Dosage
    /// </summary>
    public static void DeserializeJson(this MedicationAdministration.DosageComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationAdministration#Dosage
    /// </summary>
    public static void DeserializeJsonProperty(this MedicationAdministration.DosageComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "text":
          current.TextElement = new FhirString(reader.GetString());
          break;

        case "_text":
          ((Hl7.Fhir.Model.Element)current.TextElement).DeserializeJson(ref reader, options);
          break;

        case "site":
          current.Site = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Site).DeserializeJson(ref reader, options);
          break;

        case "route":
          current.Route = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Route).DeserializeJson(ref reader, options);
          break;

        case "method":
          current.Method = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Method).DeserializeJson(ref reader, options);
          break;

        case "dose":
          current.Dose = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.Dose).DeserializeJson(ref reader, options);
          break;

        case "rateRatio":
          current.Rate = new Hl7.Fhir.Model.Ratio();
          ((Hl7.Fhir.Model.Ratio)current.Rate).DeserializeJson(ref reader, options);
          break;

        case "rateQuantity":
          current.Rate = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.Rate).DeserializeJson(ref reader, options);
          break;

        // Complex: dosage, Export: DosageComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class MedicationAdministrationJsonConverter : JsonConverter<MedicationAdministration>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(MedicationAdministration).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, MedicationAdministration value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override MedicationAdministration Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        MedicationAdministration target = new MedicationAdministration();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
