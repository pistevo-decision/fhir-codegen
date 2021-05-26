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
  /// JSON Serialization Extensions for Patient
  /// </summary>
  public static class PatientJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR Patient into JSON
    /// </summary>
    public static void SerializeJson(this Patient current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","Patient");
      // Complex: Patient, Export: Patient, Base: DomainResource (DomainResource)
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

      if (current.ActiveElement != null)
      {
        if (current.ActiveElement.Value != null)
        {
          writer.WriteBoolean("active",(bool)current.ActiveElement.Value);
        }
        if (current.ActiveElement.HasExtensions() || (!string.IsNullOrEmpty(current.ActiveElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_active",false,current.ActiveElement.Extension,current.ActiveElement.ElementId);
        }
      }

      if ((current.Name != null) && (current.Name.Count != 0))
      {
        writer.WritePropertyName("name");
        writer.WriteStartArray();
        foreach (HumanName val in current.Name)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Telecom != null) && (current.Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();
        foreach (ContactPoint val in current.Telecom)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.GenderElement != null)
      {
        writer.WriteString("gender",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.GenderElement.Value));
      }

      if (current.BirthDateElement != null)
      {
        if (!string.IsNullOrEmpty(current.BirthDateElement.Value))
        {
          writer.WriteString("birthDate",current.BirthDateElement.Value);
        }
        if (current.BirthDateElement.HasExtensions() || (!string.IsNullOrEmpty(current.BirthDateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_birthDate",false,current.BirthDateElement.Extension,current.BirthDateElement.ElementId);
        }
      }

      if (current.Deceased != null)
      {
        switch (current.Deceased)
        {
          case FhirBoolean v_FhirBoolean:
            writer.WriteBoolean("deceasedBoolean", (bool)v_FhirBoolean.Value);
            break;
          case FhirDateTime v_FhirDateTime:
            writer.WriteString("deceasedDateTime",v_FhirDateTime.Value);
            break;
        }
      }
      if ((current.Address != null) && (current.Address.Count != 0))
      {
        writer.WritePropertyName("address");
        writer.WriteStartArray();
        foreach (Address val in current.Address)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.MaritalStatus != null)
      {
        writer.WritePropertyName("maritalStatus");
        current.MaritalStatus.SerializeJson(writer, options);
      }

      if (current.MultipleBirth != null)
      {
        switch (current.MultipleBirth)
        {
          case FhirBoolean v_FhirBoolean:
            writer.WriteBoolean("multipleBirthBoolean", (bool)v_FhirBoolean.Value);
            break;
          case Integer v_Integer:
            writer.WriteNumber("multipleBirthInteger",(int)v_Integer.Value);
            break;
        }
      }
      if ((current.Photo != null) && (current.Photo.Count != 0))
      {
        writer.WritePropertyName("photo");
        writer.WriteStartArray();
        foreach (Attachment val in current.Photo)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Contact != null) && (current.Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();
        foreach (Patient.ContactComponent val in current.Contact)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Communication != null) && (current.Communication.Count != 0))
      {
        writer.WritePropertyName("communication");
        writer.WriteStartArray();
        foreach (Patient.CommunicationComponent val in current.Communication)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.GeneralPractitioner != null) && (current.GeneralPractitioner.Count != 0))
      {
        writer.WritePropertyName("generalPractitioner");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.GeneralPractitioner)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.ManagingOrganization != null)
      {
        writer.WritePropertyName("managingOrganization");
        current.ManagingOrganization.SerializeJson(writer, options);
      }

      if ((current.Link != null) && (current.Link.Count != 0))
      {
        writer.WritePropertyName("link");
        writer.WriteStartArray();
        foreach (Patient.LinkComponent val in current.Link)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Patient
    /// </summary>
    public static void DeserializeJson(this Patient current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Patient
    /// </summary>
    public static void DeserializeJsonProperty(this Patient current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "active":
          current.ActiveElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "_active":
          ((Hl7.Fhir.Model.Element)current.ActiveElement).DeserializeJson(ref reader, options);
          break;

        case "name":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Name = new List<HumanName>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.HumanName v_Name = new Hl7.Fhir.Model.HumanName();
            v_Name.DeserializeJson(ref reader, options);
            current.Name.Add(v_Name);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Name.Count == 0)
          {
            current.Name = null;
          }
          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ContactPoint v_Telecom = new Hl7.Fhir.Model.ContactPoint();
            v_Telecom.DeserializeJson(ref reader, options);
            current.Telecom.Add(v_Telecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Telecom.Count == 0)
          {
            current.Telecom = null;
          }
          break;

        case "gender":
          current.GenderElement =new Code<Hl7.Fhir.Model.AdministrativeGender>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AdministrativeGender>(reader.GetString()));
          break;

        case "_gender":
          ((Hl7.Fhir.Model.Element)current.GenderElement).DeserializeJson(ref reader, options);
          break;

        case "birthDate":
          current.BirthDateElement = new Date(reader.GetString());
          break;

        case "_birthDate":
          ((Hl7.Fhir.Model.Element)current.BirthDateElement).DeserializeJson(ref reader, options);
          break;

        case "deceasedBoolean":
          current.Deceased = new FhirBoolean(reader.GetBoolean());
          break;

        case "deceasedDateTime":
          current.Deceased = new FhirDateTime(reader.GetString());
          break;

        case "address":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Address = new List<Address>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Address v_Address = new Hl7.Fhir.Model.Address();
            v_Address.DeserializeJson(ref reader, options);
            current.Address.Add(v_Address);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Address.Count == 0)
          {
            current.Address = null;
          }
          break;

        case "maritalStatus":
          current.MaritalStatus = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.MaritalStatus).DeserializeJson(ref reader, options);
          break;

        case "multipleBirthBoolean":
          current.MultipleBirth = new FhirBoolean(reader.GetBoolean());
          break;

        case "multipleBirthInteger":
          current.MultipleBirth = new Integer(reader.GetInt32());
          break;

        case "photo":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Photo = new List<Attachment>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Attachment v_Photo = new Hl7.Fhir.Model.Attachment();
            v_Photo.DeserializeJson(ref reader, options);
            current.Photo.Add(v_Photo);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Photo.Count == 0)
          {
            current.Photo = null;
          }
          break;

        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Contact = new List<Patient.ContactComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Patient.ContactComponent v_Contact = new Hl7.Fhir.Model.Patient.ContactComponent();
            v_Contact.DeserializeJson(ref reader, options);
            current.Contact.Add(v_Contact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Contact.Count == 0)
          {
            current.Contact = null;
          }
          break;

        case "communication":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Communication = new List<Patient.CommunicationComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Patient.CommunicationComponent v_Communication = new Hl7.Fhir.Model.Patient.CommunicationComponent();
            v_Communication.DeserializeJson(ref reader, options);
            current.Communication.Add(v_Communication);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Communication.Count == 0)
          {
            current.Communication = null;
          }
          break;

        case "generalPractitioner":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.GeneralPractitioner = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_GeneralPractitioner = new Hl7.Fhir.Model.ResourceReference();
            v_GeneralPractitioner.DeserializeJson(ref reader, options);
            current.GeneralPractitioner.Add(v_GeneralPractitioner);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.GeneralPractitioner.Count == 0)
          {
            current.GeneralPractitioner = null;
          }
          break;

        case "managingOrganization":
          current.ManagingOrganization = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.ManagingOrganization).DeserializeJson(ref reader, options);
          break;

        case "link":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Link = new List<Patient.LinkComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Patient.LinkComponent v_Link = new Hl7.Fhir.Model.Patient.LinkComponent();
            v_Link.DeserializeJson(ref reader, options);
            current.Link.Add(v_Link);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Link.Count == 0)
          {
            current.Link = null;
          }
          break;

        // Complex: Patient, Export: Patient, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Patient#Contact into JSON
    /// </summary>
    public static void SerializeJson(this Patient.ContactComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Patient#Contact, Export: ContactComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if ((current.Relationship != null) && (current.Relationship.Count != 0))
      {
        writer.WritePropertyName("relationship");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Relationship)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Name != null)
      {
        writer.WritePropertyName("name");
        current.Name.SerializeJson(writer, options);
      }

      if ((current.Telecom != null) && (current.Telecom.Count != 0))
      {
        writer.WritePropertyName("telecom");
        writer.WriteStartArray();
        foreach (ContactPoint val in current.Telecom)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Address != null)
      {
        writer.WritePropertyName("address");
        current.Address.SerializeJson(writer, options);
      }

      if (current.GenderElement != null)
      {
        writer.WriteString("gender",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.GenderElement.Value));
      }

      if (current.Organization != null)
      {
        writer.WritePropertyName("organization");
        current.Organization.SerializeJson(writer, options);
      }

      if (current.Period != null)
      {
        writer.WritePropertyName("period");
        current.Period.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Patient#Contact
    /// </summary>
    public static void DeserializeJson(this Patient.ContactComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Patient#Contact
    /// </summary>
    public static void DeserializeJsonProperty(this Patient.ContactComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "relationship":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Relationship = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Relationship = new Hl7.Fhir.Model.CodeableConcept();
            v_Relationship.DeserializeJson(ref reader, options);
            current.Relationship.Add(v_Relationship);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Relationship.Count == 0)
          {
            current.Relationship = null;
          }
          break;

        case "name":
          current.Name = new Hl7.Fhir.Model.HumanName();
          ((Hl7.Fhir.Model.HumanName)current.Name).DeserializeJson(ref reader, options);
          break;

        case "telecom":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Telecom = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ContactPoint v_Telecom = new Hl7.Fhir.Model.ContactPoint();
            v_Telecom.DeserializeJson(ref reader, options);
            current.Telecom.Add(v_Telecom);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Telecom.Count == 0)
          {
            current.Telecom = null;
          }
          break;

        case "address":
          current.Address = new Hl7.Fhir.Model.Address();
          ((Hl7.Fhir.Model.Address)current.Address).DeserializeJson(ref reader, options);
          break;

        case "gender":
          current.GenderElement =new Code<Hl7.Fhir.Model.AdministrativeGender>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.AdministrativeGender>(reader.GetString()));
          break;

        case "_gender":
          ((Hl7.Fhir.Model.Element)current.GenderElement).DeserializeJson(ref reader, options);
          break;

        case "organization":
          current.Organization = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Organization).DeserializeJson(ref reader, options);
          break;

        case "period":
          current.Period = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Period).DeserializeJson(ref reader, options);
          break;

        // Complex: contact, Export: ContactComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Patient#Communication into JSON
    /// </summary>
    public static void SerializeJson(this Patient.CommunicationComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Patient#Communication, Export: CommunicationComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WritePropertyName("language");
      current.Language.SerializeJson(writer, options);

      if (current.PreferredElement != null)
      {
        if (current.PreferredElement.Value != null)
        {
          writer.WriteBoolean("preferred",(bool)current.PreferredElement.Value);
        }
        if (current.PreferredElement.HasExtensions() || (!string.IsNullOrEmpty(current.PreferredElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_preferred",false,current.PreferredElement.Extension,current.PreferredElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Patient#Communication
    /// </summary>
    public static void DeserializeJson(this Patient.CommunicationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Patient#Communication
    /// </summary>
    public static void DeserializeJsonProperty(this Patient.CommunicationComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "language":
          current.Language = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Language).DeserializeJson(ref reader, options);
          break;

        case "preferred":
          current.PreferredElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "_preferred":
          ((Hl7.Fhir.Model.Element)current.PreferredElement).DeserializeJson(ref reader, options);
          break;

        // Complex: communication, Export: CommunicationComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR Patient#Link into JSON
    /// </summary>
    public static void SerializeJson(this Patient.LinkComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: Patient#Link, Export: LinkComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      writer.WritePropertyName("other");
      current.Other.SerializeJson(writer, options);

      writer.WriteString("type",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.TypeElement.Value));

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR Patient#Link
    /// </summary>
    public static void DeserializeJson(this Patient.LinkComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR Patient#Link
    /// </summary>
    public static void DeserializeJsonProperty(this Patient.LinkComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "other":
          current.Other = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Other).DeserializeJson(ref reader, options);
          break;

        case "type":
          current.TypeElement =new Code<Hl7.Fhir.Model.Patient.LinkType>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.Patient.LinkType>(reader.GetString()));
          break;

        case "_type":
          ((Hl7.Fhir.Model.Element)current.TypeElement).DeserializeJson(ref reader, options);
          break;

        // Complex: link, Export: LinkComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class PatientJsonConverter : JsonConverter<Patient>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(Patient).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, Patient value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override Patient Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        Patient target = new Patient();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
