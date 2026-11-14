# Generates the Python phonenumbers==9.0.43 reference fixtures in
# tests/testthat/_fixtures/ from data-raw/upstream/PhoneNumberMetadata.xml.
import csv
import random
import xml.etree.ElementTree as ET
import phonenumbers
from phonenumbers import PhoneNumberFormat, PhoneNumberType, NumberParseException

assert phonenumbers.__version__ == "9.0.43", f"Expected 9.0.43, got {phonenumbers.__version__}"
random.seed(20261114)

tree = ET.parse("data-raw/upstream/PhoneNumberMetadata.xml")
root = tree.getroot()
TYPE_TAGS = [
    "fixedLine", "mobile", "tollFree", "premiumRate", "sharedCost",
    "personalNumber", "voip", "pager", "uan", "voicemail"
]

examples = []
regions = []
for t in root.findall(".//territory"):
    tid = t.attrib["id"]
    cc = int(t.attrib["countryCode"])
    if tid != "001":
        regions.append(tid)
    for tag in TYPE_TAGS:
        el = t.find(tag)
        if el is not None and el.findtext("exampleNumber"):
            examples.append((tid, cc, tag, el.findtext("exampleNumber")))


def eval_case(s, reg):
    r_arg = reg if reg != "" else None
    try:
        p = phonenumbers.parse(s, r_arg)
        p_cc = str(p.country_code)
        nsn = phonenumbers.national_significant_number(p)
        ext = p.extension or ""
        poss = "TRUE" if phonenumbers.is_possible_number(p) else "FALSE"
        val = "TRUE" if phonenumbers.is_valid_number(p) else "FALSE"
        tp = PhoneNumberType.to_string(phonenumbers.number_type(p)).lower()
        rc = phonenumbers.region_code_for_number(p) or ""
        f_e164 = phonenumbers.format_number(p, PhoneNumberFormat.E164)
        f_intl = phonenumbers.format_number(p, PhoneNumberFormat.INTERNATIONAL)
        f_nat = phonenumbers.format_number(p, PhoneNumberFormat.NATIONAL)
        f_rfc = phonenumbers.format_number(p, PhoneNumberFormat.RFC3966)
    except NumberParseException:
        p_cc = nsn = ext = tp = rc = f_e164 = f_intl = f_nat = f_rfc = ""
        poss = val = "FALSE"
    return (s, reg, p_cc, nsn, ext, poss, val, tp, rc, f_e164, f_intl, f_nat, f_rfc)
