// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Indicates more detailed reason for outcome.
  /// </summary>
  public static class AuditEventOutcomeDetailCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding DELETEMULTIPLEMATCHES = new Coding
    {
      Code = "DELETE_MULTIPLE_MATCHES",
      Display = "Error: Multiple matches exist for the conditional delete",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGAUTHREQUIRED = new Coding
    {
      Code = "MSG_AUTH_REQUIRED",
      Display = "You must authenticate before you can use this service",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGBADFORMAT = new Coding
    {
      Code = "MSG_BAD_FORMAT",
      Display = "Bad Syntax: \"%s\" must be a %s'",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGBADSYNTAX = new Coding
    {
      Code = "MSG_BAD_SYNTAX",
      Display = "Bad Syntax in %s",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGCANTPARSECONTENT = new Coding
    {
      Code = "MSG_CANT_PARSE_CONTENT",
      Display = "Unable to parse feed (entry content type = \"%s\")",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGCANTPARSEROOT = new Coding
    {
      Code = "MSG_CANT_PARSE_ROOT",
      Display = "Unable to parse feed (root element name = \"%s\")",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGCREATED = new Coding
    {
      Code = "MSG_CREATED",
      Display = "New resource created",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGDATEFORMAT = new Coding
    {
      Code = "MSG_DATE_FORMAT",
      Display = "The Date value %s is not in the correct format (Xml Date Format required)",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGDELETED = new Coding
    {
      Code = "MSG_DELETED",
      Display = "This resource has been deleted",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGDELETEDDONE = new Coding
    {
      Code = "MSG_DELETED_DONE",
      Display = "Resource deleted",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGDELETEDID = new Coding
    {
      Code = "MSG_DELETED_ID",
      Display = "The resource \"%s\" has been deleted",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGDUPLICATEID = new Coding
    {
      Code = "MSG_DUPLICATE_ID",
      Display = "Duplicate Id %s for resource type %s",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGERRORPARSING = new Coding
    {
      Code = "MSG_ERROR_PARSING",
      Display = "Error parsing resource Xml (%s)",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGIDINVALID = new Coding
    {
      Code = "MSG_ID_INVALID",
      Display = "Id \"%s\" has an invalid character \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGIDTOOLONG = new Coding
    {
      Code = "MSG_ID_TOO_LONG",
      Display = "Id \"%s\" too long (length limit 36)",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGINVALIDID = new Coding
    {
      Code = "MSG_INVALID_ID",
      Display = "Id not accepted",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGJSONOBJECT = new Coding
    {
      Code = "MSG_JSON_OBJECT",
      Display = "Json Source for a resource should start with an object",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGLOCALFAIL = new Coding
    {
      Code = "MSG_LOCAL_FAIL",
      Display = "Unable to resolve local reference to resource %s",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGNOEXIST = new Coding
    {
      Code = "MSG_NO_EXIST",
      Display = "Resource Id \"%s\" does not exist",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGNOMATCH = new Coding
    {
      Code = "MSG_NO_MATCH",
      Display = "No Resource found matching the query \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGNOMODULE = new Coding
    {
      Code = "MSG_NO_MODULE",
      Display = "No module could be found to handle the request \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGNOSUMMARY = new Coding
    {
      Code = "MSG_NO_SUMMARY",
      Display = "No Summary for this resource",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGOPNOTALLOWED = new Coding
    {
      Code = "MSG_OP_NOT_ALLOWED",
      Display = "Operation %s not allowed for resource %s (due to local configuration)",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGPARAMCHAINED = new Coding
    {
      Code = "MSG_PARAM_CHAINED",
      Display = "Unknown chained parameter name \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGPARAMINVALID = new Coding
    {
      Code = "MSG_PARAM_INVALID",
      Display = "Parameter \"%s\" content is invalid",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGPARAMMODIFIERINVALID = new Coding
    {
      Code = "MSG_PARAM_MODIFIER_INVALID",
      Display = "Parameter \"%s\" modifier is invalid",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGPARAMNOREPEAT = new Coding
    {
      Code = "MSG_PARAM_NO_REPEAT",
      Display = "Parameter \"%s\" is not allowed to repeat",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGPARAMUNKNOWN = new Coding
    {
      Code = "MSG_PARAM_UNKNOWN",
      Display = "Parameter \"%s\" not understood",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCEEXAMPLEPROTECTED = new Coding
    {
      Code = "MSG_RESOURCE_EXAMPLE_PROTECTED",
      Display = "Resources with identity \"example\" cannot be deleted (for testing/training purposes)",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCEIDFAIL = new Coding
    {
      Code = "MSG_RESOURCE_ID_FAIL",
      Display = "unable to allocate resource id",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCEIDMISMATCH = new Coding
    {
      Code = "MSG_RESOURCE_ID_MISMATCH",
      Display = "Resource Id Mismatch",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCEIDMISSING = new Coding
    {
      Code = "MSG_RESOURCE_ID_MISSING",
      Display = "Resource Id Missing",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCENOTALLOWED = new Coding
    {
      Code = "MSG_RESOURCE_NOT_ALLOWED",
      Display = "Not allowed to submit a resource for this operation",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCEREQUIRED = new Coding
    {
      Code = "MSG_RESOURCE_REQUIRED",
      Display = "A resource is required",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGRESOURCETYPEMISMATCH = new Coding
    {
      Code = "MSG_RESOURCE_TYPE_MISMATCH",
      Display = "Resource Type Mismatch",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGSORTUNKNOWN = new Coding
    {
      Code = "MSG_SORT_UNKNOWN",
      Display = "Unknown sort parameter name \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGTRANSACTIONDUPLICATEID = new Coding
    {
      Code = "MSG_TRANSACTION_DUPLICATE_ID",
      Display = "Duplicate Identifier in transaction: %s",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGTRANSACTIONMISSINGID = new Coding
    {
      Code = "MSG_TRANSACTION_MISSING_ID",
      Display = "Missing Identifier in transaction - an entry.id must be provided",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGUNHANDLEDNODETYPE = new Coding
    {
      Code = "MSG_UNHANDLED_NODE_TYPE",
      Display = "Unhandled xml node type \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGUNKNOWNCONTENT = new Coding
    {
      Code = "MSG_UNKNOWN_CONTENT",
      Display = "Unknown Content (%s) at %s",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGUNKNOWNOPERATION = new Coding
    {
      Code = "MSG_UNKNOWN_OPERATION",
      Display = "unknown FHIR http operation",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGUNKNOWNTYPE = new Coding
    {
      Code = "MSG_UNKNOWN_TYPE",
      Display = "Resource Type \"%s\" not recognised",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGUPDATED = new Coding
    {
      Code = "MSG_UPDATED",
      Display = "existing resource updated",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGVERSIONAWARE = new Coding
    {
      Code = "MSG_VERSION_AWARE",
      Display = "Version aware updates are required for this resource",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGVERSIONAWARECONFLICT = new Coding
    {
      Code = "MSG_VERSION_AWARE_CONFLICT",
      Display = "Update Conflict (server current version = \"%s\", client version referenced = \"%s\")",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGVERSIONAWAREURL = new Coding
    {
      Code = "MSG_VERSION_AWARE_URL",
      Display = "Version specific URL not recognised",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding MSGWRONGNS = new Coding
    {
      Code = "MSG_WRONG_NS",
      Display = "This does not appear to be a FHIR element or resource (wrong namespace \"%s\")",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding SEARCHMULTIPLE = new Coding
    {
      Code = "SEARCH_MULTIPLE",
      Display = "Error: Multiple matches exist for %s search parameters \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding SEARCHNONE = new Coding
    {
      Code = "SEARCH_NONE",
      Display = "Error: no processable search found for %s search parameters \"%s\"",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding UPDATEMULTIPLEMATCHES = new Coding
    {
      Code = "UPDATE_MULTIPLE_MATCHES",
      Display = "Error: Multiple matches exist for the conditional update",
      System = "http://terminology.hl7.org/CodeSystem/operation-outcome"
    };

    /// <summary>
    /// Literal for code: DELETEMULTIPLEMATCHES
    /// </summary>
    public const string LiteralDELETEMULTIPLEMATCHES = "DELETE_MULTIPLE_MATCHES";

    /// <summary>
    /// Literal for code: MSGAUTHREQUIRED
    /// </summary>
    public const string LiteralMSGAUTHREQUIRED = "MSG_AUTH_REQUIRED";

    /// <summary>
    /// Literal for code: MSGBADFORMAT
    /// </summary>
    public const string LiteralMSGBADFORMAT = "MSG_BAD_FORMAT";

    /// <summary>
    /// Literal for code: MSGBADSYNTAX
    /// </summary>
    public const string LiteralMSGBADSYNTAX = "MSG_BAD_SYNTAX";

    /// <summary>
    /// Literal for code: MSGCANTPARSECONTENT
    /// </summary>
    public const string LiteralMSGCANTPARSECONTENT = "MSG_CANT_PARSE_CONTENT";

    /// <summary>
    /// Literal for code: MSGCANTPARSEROOT
    /// </summary>
    public const string LiteralMSGCANTPARSEROOT = "MSG_CANT_PARSE_ROOT";

    /// <summary>
    /// Literal for code: MSGCREATED
    /// </summary>
    public const string LiteralMSGCREATED = "MSG_CREATED";

    /// <summary>
    /// Literal for code: MSGDATEFORMAT
    /// </summary>
    public const string LiteralMSGDATEFORMAT = "MSG_DATE_FORMAT";

    /// <summary>
    /// Literal for code: MSGDELETED
    /// </summary>
    public const string LiteralMSGDELETED = "MSG_DELETED";

    /// <summary>
    /// Literal for code: MSGDELETEDDONE
    /// </summary>
    public const string LiteralMSGDELETEDDONE = "MSG_DELETED_DONE";

    /// <summary>
    /// Literal for code: MSGDELETEDID
    /// </summary>
    public const string LiteralMSGDELETEDID = "MSG_DELETED_ID";

    /// <summary>
    /// Literal for code: MSGDUPLICATEID
    /// </summary>
    public const string LiteralMSGDUPLICATEID = "MSG_DUPLICATE_ID";

    /// <summary>
    /// Literal for code: MSGERRORPARSING
    /// </summary>
    public const string LiteralMSGERRORPARSING = "MSG_ERROR_PARSING";

    /// <summary>
    /// Literal for code: MSGIDINVALID
    /// </summary>
    public const string LiteralMSGIDINVALID = "MSG_ID_INVALID";

    /// <summary>
    /// Literal for code: MSGIDTOOLONG
    /// </summary>
    public const string LiteralMSGIDTOOLONG = "MSG_ID_TOO_LONG";

    /// <summary>
    /// Literal for code: MSGINVALIDID
    /// </summary>
    public const string LiteralMSGINVALIDID = "MSG_INVALID_ID";

    /// <summary>
    /// Literal for code: MSGJSONOBJECT
    /// </summary>
    public const string LiteralMSGJSONOBJECT = "MSG_JSON_OBJECT";

    /// <summary>
    /// Literal for code: MSGLOCALFAIL
    /// </summary>
    public const string LiteralMSGLOCALFAIL = "MSG_LOCAL_FAIL";

    /// <summary>
    /// Literal for code: MSGNOEXIST
    /// </summary>
    public const string LiteralMSGNOEXIST = "MSG_NO_EXIST";

    /// <summary>
    /// Literal for code: MSGNOMATCH
    /// </summary>
    public const string LiteralMSGNOMATCH = "MSG_NO_MATCH";

    /// <summary>
    /// Literal for code: MSGNOMODULE
    /// </summary>
    public const string LiteralMSGNOMODULE = "MSG_NO_MODULE";

    /// <summary>
    /// Literal for code: MSGNOSUMMARY
    /// </summary>
    public const string LiteralMSGNOSUMMARY = "MSG_NO_SUMMARY";

    /// <summary>
    /// Literal for code: MSGOPNOTALLOWED
    /// </summary>
    public const string LiteralMSGOPNOTALLOWED = "MSG_OP_NOT_ALLOWED";

    /// <summary>
    /// Literal for code: MSGPARAMCHAINED
    /// </summary>
    public const string LiteralMSGPARAMCHAINED = "MSG_PARAM_CHAINED";

    /// <summary>
    /// Literal for code: MSGPARAMINVALID
    /// </summary>
    public const string LiteralMSGPARAMINVALID = "MSG_PARAM_INVALID";

    /// <summary>
    /// Literal for code: MSGPARAMMODIFIERINVALID
    /// </summary>
    public const string LiteralMSGPARAMMODIFIERINVALID = "MSG_PARAM_MODIFIER_INVALID";

    /// <summary>
    /// Literal for code: MSGPARAMNOREPEAT
    /// </summary>
    public const string LiteralMSGPARAMNOREPEAT = "MSG_PARAM_NO_REPEAT";

    /// <summary>
    /// Literal for code: MSGPARAMUNKNOWN
    /// </summary>
    public const string LiteralMSGPARAMUNKNOWN = "MSG_PARAM_UNKNOWN";

    /// <summary>
    /// Literal for code: MSGRESOURCEEXAMPLEPROTECTED
    /// </summary>
    public const string LiteralMSGRESOURCEEXAMPLEPROTECTED = "MSG_RESOURCE_EXAMPLE_PROTECTED";

    /// <summary>
    /// Literal for code: MSGRESOURCEIDFAIL
    /// </summary>
    public const string LiteralMSGRESOURCEIDFAIL = "MSG_RESOURCE_ID_FAIL";

    /// <summary>
    /// Literal for code: MSGRESOURCEIDMISMATCH
    /// </summary>
    public const string LiteralMSGRESOURCEIDMISMATCH = "MSG_RESOURCE_ID_MISMATCH";

    /// <summary>
    /// Literal for code: MSGRESOURCEIDMISSING
    /// </summary>
    public const string LiteralMSGRESOURCEIDMISSING = "MSG_RESOURCE_ID_MISSING";

    /// <summary>
    /// Literal for code: MSGRESOURCENOTALLOWED
    /// </summary>
    public const string LiteralMSGRESOURCENOTALLOWED = "MSG_RESOURCE_NOT_ALLOWED";

    /// <summary>
    /// Literal for code: MSGRESOURCEREQUIRED
    /// </summary>
    public const string LiteralMSGRESOURCEREQUIRED = "MSG_RESOURCE_REQUIRED";

    /// <summary>
    /// Literal for code: MSGRESOURCETYPEMISMATCH
    /// </summary>
    public const string LiteralMSGRESOURCETYPEMISMATCH = "MSG_RESOURCE_TYPE_MISMATCH";

    /// <summary>
    /// Literal for code: MSGSORTUNKNOWN
    /// </summary>
    public const string LiteralMSGSORTUNKNOWN = "MSG_SORT_UNKNOWN";

    /// <summary>
    /// Literal for code: MSGTRANSACTIONDUPLICATEID
    /// </summary>
    public const string LiteralMSGTRANSACTIONDUPLICATEID = "MSG_TRANSACTION_DUPLICATE_ID";

    /// <summary>
    /// Literal for code: MSGTRANSACTIONMISSINGID
    /// </summary>
    public const string LiteralMSGTRANSACTIONMISSINGID = "MSG_TRANSACTION_MISSING_ID";

    /// <summary>
    /// Literal for code: MSGUNHANDLEDNODETYPE
    /// </summary>
    public const string LiteralMSGUNHANDLEDNODETYPE = "MSG_UNHANDLED_NODE_TYPE";

    /// <summary>
    /// Literal for code: MSGUNKNOWNCONTENT
    /// </summary>
    public const string LiteralMSGUNKNOWNCONTENT = "MSG_UNKNOWN_CONTENT";

    /// <summary>
    /// Literal for code: MSGUNKNOWNOPERATION
    /// </summary>
    public const string LiteralMSGUNKNOWNOPERATION = "MSG_UNKNOWN_OPERATION";

    /// <summary>
    /// Literal for code: MSGUNKNOWNTYPE
    /// </summary>
    public const string LiteralMSGUNKNOWNTYPE = "MSG_UNKNOWN_TYPE";

    /// <summary>
    /// Literal for code: MSGUPDATED
    /// </summary>
    public const string LiteralMSGUPDATED = "MSG_UPDATED";

    /// <summary>
    /// Literal for code: MSGVERSIONAWARE
    /// </summary>
    public const string LiteralMSGVERSIONAWARE = "MSG_VERSION_AWARE";

    /// <summary>
    /// Literal for code: MSGVERSIONAWARECONFLICT
    /// </summary>
    public const string LiteralMSGVERSIONAWARECONFLICT = "MSG_VERSION_AWARE_CONFLICT";

    /// <summary>
    /// Literal for code: MSGVERSIONAWAREURL
    /// </summary>
    public const string LiteralMSGVERSIONAWAREURL = "MSG_VERSION_AWARE_URL";

    /// <summary>
    /// Literal for code: MSGWRONGNS
    /// </summary>
    public const string LiteralMSGWRONGNS = "MSG_WRONG_NS";

    /// <summary>
    /// Literal for code: SEARCHMULTIPLE
    /// </summary>
    public const string LiteralSEARCHMULTIPLE = "SEARCH_MULTIPLE";

    /// <summary>
    /// Literal for code: SEARCHNONE
    /// </summary>
    public const string LiteralSEARCHNONE = "SEARCH_NONE";

    /// <summary>
    /// Literal for code: UPDATEMULTIPLEMATCHES
    /// </summary>
    public const string LiteralUPDATEMULTIPLEMATCHES = "UPDATE_MULTIPLE_MATCHES";
  };
}
