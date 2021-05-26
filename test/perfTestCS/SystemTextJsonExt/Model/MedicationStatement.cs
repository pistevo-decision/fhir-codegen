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
  /// JSON Serialization Extensions for MedicationStatement
  /// </summary>
  public static class MedicationStatementJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR MedicationStatement into JSON
    /// </summary>
    public static void SerializeJson(this MedicationStatement current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","MedicationStatement");
      // Complex: MedicationStatement, Export: MedicationStatement, Base: DomainResource (DomainResource)
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

      if ((current.BasedOn != null) && (current.BasedOn.Count != 0))
      {
        writer.WritePropertyName("basedOn");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.BasedOn)
        {
          val.SerializeJson(writer, options, true);
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
      if (current.DateAssertedElement != null)
      {
        if (!string.IsNullOrEmpty(current.DateAssertedElement.Value))
        {
          writer.WriteString("dateAsserted",current.DateAssertedElement.Value);
        }
        if (current.DateAssertedElement.HasExtensions() || (!string.IsNullOrEmpty(current.DateAssertedElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_dateAsserted",false,current.DateAssertedElement.Extension,current.DateAssertedElement.ElementId);
        }
      }

      if (current.InformationSource != null)
      {
        writer.WritePropertyName("informationSource");
        current.InformationSource.SerializeJson(writer, options);
      }

      if ((current.DerivedFrom != null) && (current.DerivedFrom.Count != 0))
      {
        writer.WritePropertyName("derivedFrom");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.DerivedFrom)
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

      if ((current.Dosage != null) && (current.Dosage.Count != 0))
      {
        writer.WritePropertyName("dosage");
        writer.WriteStartArray();
        foreach (Dosage val in current.Dosage)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicationStatement
    /// </summary>
    public static void DeserializeJson(this MedicationStatement current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR MedicationStatement
    /// </summary>
    public static void DeserializeJsonProperty(this MedicationStatement current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "basedOn":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.BasedOn = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_BasedOn = new Hl7.Fhir.Model.ResourceReference();
            v_BasedOn.DeserializeJson(ref reader, options);
            current.BasedOn.Add(v_BasedOn);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.BasedOn.Count == 0)
          {
            current.BasedOn = null;
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
          current.StatusElement =new Code<Hl7.Fhir.Model.MedicationStatement.MedicationStatusCodes>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.MedicationStatement.MedicationStatusCodes>(reader.GetString()));
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

        case "effectiveDateTime":
          current.Effective = new FhirDateTime(reader.GetString());
          break;

        case "effectivePeriod":
          current.Effective = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Effective).DeserializeJson(ref reader, options);
          break;

        case "dateAsserted":
          current.DateAssertedElement = new FhirDateTime(reader.GetString());
          break;

        case "_dateAsserted":
          ((Hl7.Fhir.Model.Element)current.DateAssertedElement).DeserializeJson(ref reader, options);
          break;

        case "informationSource":
          current.InformationSource = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.InformationSource).DeserializeJson(ref reader, options);
          break;

        case "derivedFrom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.DerivedFrom = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_DerivedFrom = new Hl7.Fhir.Model.ResourceReference();
            v_DerivedFrom.DeserializeJson(ref reader, options);
            current.DerivedFrom.Add(v_DerivedFrom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.DerivedFrom.Count == 0)
          {
            current.DerivedFrom = null;
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
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Dosage = new List<Dosage>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Dosage v_Dosage = new Hl7.Fhir.Model.Dosage();
            v_Dosage.DeserializeJson(ref reader, options);
            current.Dosage.Add(v_Dosage);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Dosage.Count == 0)
          {
            current.Dosage = null;
          }
          break;

        // Complex: MedicationStatement, Export: MedicationStatement, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class MedicationStatementJsonConverter : JsonConverter<MedicationStatement>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(MedicationStatement).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, MedicationStatement value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override MedicationStatement Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        MedicationStatement target = new MedicationStatement();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
