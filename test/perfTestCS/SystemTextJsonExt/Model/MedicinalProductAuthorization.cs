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
  /// JSON Serialization Extensions for MedicinalProductAuthorization
  /// </summary>
  public static class MedicinalProductAuthorizationJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR MedicinalProductAuthorization into JSON
    /// </summary>
    public static void SerializeJson(this MedicinalProductAuthorization current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","MedicinalProductAuthorization");
      // Complex: MedicinalProductAuthorization, Export: MedicinalProductAuthorization, Base: DomainResource (DomainResource)
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

      if (current.Subject != null)
      {
        writer.WritePropertyName("subject");
        current.Subject.SerializeJson(writer, options);
      }

      if ((current.Country != null) && (current.Country.Count != 0))
      {
        writer.WritePropertyName("country");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Country)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Jurisdiction != null) && (current.Jurisdiction.Count != 0))
      {
        writer.WritePropertyName("jurisdiction");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Jurisdiction)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Status != null)
      {
        writer.WritePropertyName("status");
        current.Status.SerializeJson(writer, options);
      }

      if (current.StatusDateElement != null)
      {
        if (!string.IsNullOrEmpty(current.StatusDateElement.Value))
        {
          writer.WriteString("statusDate",current.StatusDateElement.Value);
        }
        if (current.StatusDateElement.HasExtensions() || (!string.IsNullOrEmpty(current.StatusDateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_statusDate",false,current.StatusDateElement.Extension,current.StatusDateElement.ElementId);
        }
      }

      if (current.RestoreDateElement != null)
      {
        if (!string.IsNullOrEmpty(current.RestoreDateElement.Value))
        {
          writer.WriteString("restoreDate",current.RestoreDateElement.Value);
        }
        if (current.RestoreDateElement.HasExtensions() || (!string.IsNullOrEmpty(current.RestoreDateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_restoreDate",false,current.RestoreDateElement.Extension,current.RestoreDateElement.ElementId);
        }
      }

      if (current.ValidityPeriod != null)
      {
        writer.WritePropertyName("validityPeriod");
        current.ValidityPeriod.SerializeJson(writer, options);
      }

      if (current.DataExclusivityPeriod != null)
      {
        writer.WritePropertyName("dataExclusivityPeriod");
        current.DataExclusivityPeriod.SerializeJson(writer, options);
      }

      if (current.DateOfFirstAuthorizationElement != null)
      {
        if (!string.IsNullOrEmpty(current.DateOfFirstAuthorizationElement.Value))
        {
          writer.WriteString("dateOfFirstAuthorization",current.DateOfFirstAuthorizationElement.Value);
        }
        if (current.DateOfFirstAuthorizationElement.HasExtensions() || (!string.IsNullOrEmpty(current.DateOfFirstAuthorizationElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_dateOfFirstAuthorization",false,current.DateOfFirstAuthorizationElement.Extension,current.DateOfFirstAuthorizationElement.ElementId);
        }
      }

      if (current.InternationalBirthDateElement != null)
      {
        if (!string.IsNullOrEmpty(current.InternationalBirthDateElement.Value))
        {
          writer.WriteString("internationalBirthDate",current.InternationalBirthDateElement.Value);
        }
        if (current.InternationalBirthDateElement.HasExtensions() || (!string.IsNullOrEmpty(current.InternationalBirthDateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_internationalBirthDate",false,current.InternationalBirthDateElement.Extension,current.InternationalBirthDateElement.ElementId);
        }
      }

      if (current.LegalBasis != null)
      {
        writer.WritePropertyName("legalBasis");
        current.LegalBasis.SerializeJson(writer, options);
      }

      if ((current.JurisdictionalAuthorization != null) && (current.JurisdictionalAuthorization.Count != 0))
      {
        writer.WritePropertyName("jurisdictionalAuthorization");
        writer.WriteStartArray();
        foreach (MedicinalProductAuthorization.JurisdictionalAuthorizationComponent val in current.JurisdictionalAuthorization)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Holder != null)
      {
        writer.WritePropertyName("holder");
        current.Holder.SerializeJson(writer, options);
      }

      if (current.Regulator != null)
      {
        writer.WritePropertyName("regulator");
        current.Regulator.SerializeJson(writer, options);
      }

      if (current.Procedure != null)
      {
        writer.WritePropertyName("procedure");
        current.Procedure.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicinalProductAuthorization
    /// </summary>
    public static void DeserializeJson(this MedicinalProductAuthorization current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR MedicinalProductAuthorization
    /// </summary>
    public static void DeserializeJsonProperty(this MedicinalProductAuthorization current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "subject":
          current.Subject = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Subject).DeserializeJson(ref reader, options);
          break;

        case "country":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Country = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Country = new Hl7.Fhir.Model.CodeableConcept();
            v_Country.DeserializeJson(ref reader, options);
            current.Country.Add(v_Country);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Country.Count == 0)
          {
            current.Country = null;
          }
          break;

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Jurisdiction = new Hl7.Fhir.Model.CodeableConcept();
            v_Jurisdiction.DeserializeJson(ref reader, options);
            current.Jurisdiction.Add(v_Jurisdiction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Jurisdiction.Count == 0)
          {
            current.Jurisdiction = null;
          }
          break;

        case "status":
          current.Status = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Status).DeserializeJson(ref reader, options);
          break;

        case "statusDate":
          current.StatusDateElement = new FhirDateTime(reader.GetString());
          break;

        case "_statusDate":
          ((Hl7.Fhir.Model.Element)current.StatusDateElement).DeserializeJson(ref reader, options);
          break;

        case "restoreDate":
          current.RestoreDateElement = new FhirDateTime(reader.GetString());
          break;

        case "_restoreDate":
          ((Hl7.Fhir.Model.Element)current.RestoreDateElement).DeserializeJson(ref reader, options);
          break;

        case "validityPeriod":
          current.ValidityPeriod = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.ValidityPeriod).DeserializeJson(ref reader, options);
          break;

        case "dataExclusivityPeriod":
          current.DataExclusivityPeriod = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.DataExclusivityPeriod).DeserializeJson(ref reader, options);
          break;

        case "dateOfFirstAuthorization":
          current.DateOfFirstAuthorizationElement = new FhirDateTime(reader.GetString());
          break;

        case "_dateOfFirstAuthorization":
          ((Hl7.Fhir.Model.Element)current.DateOfFirstAuthorizationElement).DeserializeJson(ref reader, options);
          break;

        case "internationalBirthDate":
          current.InternationalBirthDateElement = new FhirDateTime(reader.GetString());
          break;

        case "_internationalBirthDate":
          ((Hl7.Fhir.Model.Element)current.InternationalBirthDateElement).DeserializeJson(ref reader, options);
          break;

        case "legalBasis":
          current.LegalBasis = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.LegalBasis).DeserializeJson(ref reader, options);
          break;

        case "jurisdictionalAuthorization":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.JurisdictionalAuthorization = new List<MedicinalProductAuthorization.JurisdictionalAuthorizationComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.MedicinalProductAuthorization.JurisdictionalAuthorizationComponent v_JurisdictionalAuthorization = new Hl7.Fhir.Model.MedicinalProductAuthorization.JurisdictionalAuthorizationComponent();
            v_JurisdictionalAuthorization.DeserializeJson(ref reader, options);
            current.JurisdictionalAuthorization.Add(v_JurisdictionalAuthorization);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.JurisdictionalAuthorization.Count == 0)
          {
            current.JurisdictionalAuthorization = null;
          }
          break;

        case "holder":
          current.Holder = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Holder).DeserializeJson(ref reader, options);
          break;

        case "regulator":
          current.Regulator = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Regulator).DeserializeJson(ref reader, options);
          break;

        case "procedure":
          current.Procedure = new Hl7.Fhir.Model.MedicinalProductAuthorization.ProcedureComponent();
          ((Hl7.Fhir.Model.MedicinalProductAuthorization.ProcedureComponent)current.Procedure).DeserializeJson(ref reader, options);
          break;

        // Complex: MedicinalProductAuthorization, Export: MedicinalProductAuthorization, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR MedicinalProductAuthorization#JurisdictionalAuthorization into JSON
    /// </summary>
    public static void SerializeJson(this MedicinalProductAuthorization.JurisdictionalAuthorizationComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: MedicinalProductAuthorization#JurisdictionalAuthorization, Export: JurisdictionalAuthorizationComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

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

      if (current.Country != null)
      {
        writer.WritePropertyName("country");
        current.Country.SerializeJson(writer, options);
      }

      if ((current.Jurisdiction != null) && (current.Jurisdiction.Count != 0))
      {
        writer.WritePropertyName("jurisdiction");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Jurisdiction)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.LegalStatusOfSupply != null)
      {
        writer.WritePropertyName("legalStatusOfSupply");
        current.LegalStatusOfSupply.SerializeJson(writer, options);
      }

      if (current.ValidityPeriod != null)
      {
        writer.WritePropertyName("validityPeriod");
        current.ValidityPeriod.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicinalProductAuthorization#JurisdictionalAuthorization
    /// </summary>
    public static void DeserializeJson(this MedicinalProductAuthorization.JurisdictionalAuthorizationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR MedicinalProductAuthorization#JurisdictionalAuthorization
    /// </summary>
    public static void DeserializeJsonProperty(this MedicinalProductAuthorization.JurisdictionalAuthorizationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "country":
          current.Country = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Country).DeserializeJson(ref reader, options);
          break;

        case "jurisdiction":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Jurisdiction = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Jurisdiction = new Hl7.Fhir.Model.CodeableConcept();
            v_Jurisdiction.DeserializeJson(ref reader, options);
            current.Jurisdiction.Add(v_Jurisdiction);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Jurisdiction.Count == 0)
          {
            current.Jurisdiction = null;
          }
          break;

        case "legalStatusOfSupply":
          current.LegalStatusOfSupply = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.LegalStatusOfSupply).DeserializeJson(ref reader, options);
          break;

        case "validityPeriod":
          current.ValidityPeriod = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.ValidityPeriod).DeserializeJson(ref reader, options);
          break;

        // Complex: jurisdictionalAuthorization, Export: JurisdictionalAuthorizationComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR MedicinalProductAuthorization#Procedure into JSON
    /// </summary>
    public static void SerializeJson(this MedicinalProductAuthorization.ProcedureComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: MedicinalProductAuthorization#Procedure, Export: ProcedureComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Identifier != null)
      {
        writer.WritePropertyName("identifier");
        current.Identifier.SerializeJson(writer, options);
      }

      writer.WritePropertyName("type");
      current.Type.SerializeJson(writer, options);

      if (current.Date != null)
      {
        switch (current.Date)
        {
          case Period v_Period:
            writer.WritePropertyName("datePeriod");
            v_Period.SerializeJson(writer, options);
            break;
          case FhirDateTime v_FhirDateTime:
            writer.WriteString("dateDateTime",v_FhirDateTime.Value);
            break;
        }
      }
      if ((current.Application != null) && (current.Application.Count != 0))
      {
        writer.WritePropertyName("application");
        writer.WriteStartArray();
        foreach (MedicinalProductAuthorization.ProcedureComponent val in current.Application)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR MedicinalProductAuthorization#Procedure
    /// </summary>
    public static void DeserializeJson(this MedicinalProductAuthorization.ProcedureComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR MedicinalProductAuthorization#Procedure
    /// </summary>
    public static void DeserializeJsonProperty(this MedicinalProductAuthorization.ProcedureComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          current.Identifier = new Hl7.Fhir.Model.Identifier();
          ((Hl7.Fhir.Model.Identifier)current.Identifier).DeserializeJson(ref reader, options);
          break;

        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "datePeriod":
          current.Date = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Date).DeserializeJson(ref reader, options);
          break;

        case "dateDateTime":
          current.Date = new FhirDateTime(reader.GetString());
          break;

        case "application":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Application = new List<MedicinalProductAuthorization.ProcedureComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.MedicinalProductAuthorization.ProcedureComponent v_Application = new Hl7.Fhir.Model.MedicinalProductAuthorization.ProcedureComponent();
            v_Application.DeserializeJson(ref reader, options);
            current.Application.Add(v_Application);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Application.Count == 0)
          {
            current.Application = null;
          }
          break;

        // Complex: procedure, Export: ProcedureComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class MedicinalProductAuthorizationJsonConverter : JsonConverter<MedicinalProductAuthorization>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(MedicinalProductAuthorization).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, MedicinalProductAuthorization value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override MedicinalProductAuthorization Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        MedicinalProductAuthorization target = new MedicinalProductAuthorization();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
