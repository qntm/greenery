import pytest
from nfa import nfa_simulate, regex_to_nfa


@pytest.mark.parametrize("regex, string, expected", [
    # Test cases for matching specific characters
    ("abc", "abc", True),
    ("abc", "abcd", False),
    ("abc", "ab", False),
    # Test cases for matching digit characters
    (r"\d+", "123", True),
    (r"\d+", "abc", False),
    (r"\d+", "123abc", False),
    # Test cases for matching word characters
    (r"\w+", "helloWorld", True),
    (r"\w+", "hello World", False),
    # Test cases for matching whitespace characters
    (r"\s+", "   ", True),
    (r"\s+", "  a ", False),
    # Test cases for matching repetition
    ("a{3}", "aaa", True),
    ("a{3}", "aa", False),
    ("a{2,4}", "aaa", True),
    ("a{2,4}", "a", False),
    # Test cases for matching character sets
    ("[aeiou]", "a", True),
    ("[aeiou]", "b", False),
    # Test cases for matching either or
    ("a|b", "a", True),
    ("a|b", "c", False),
    # Test cases for matching start and end of string
    ("a.*b", "acb", True),
    ("a.*b", "ac", False),
    # Test case for matching email addresses
    (r"[\w.\-]+@[a-zA-Z\d.\-]+\.[a-zA-Z]+", "john.doe@example.com", True),
    (r"[\w.\-]+@[a-zA-Z\d.\-]+\.[a-zA-Z]+", "john.doe.example.com", False),
    # Test case for matching hexadecimal numbers
    (r"0[xX][0-9a-fA-F]+", "0x1A3f", True),
    (r"0[xX][0-9a-fA-F]+", "0x1AGT", False),
    # Test case for matching IP addresses
    (r"((25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(25[0-5]|2[0-4]\d|[01]?\d\d?)", "192.168.1.1", True),
    (r"((25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(25[0-5]|2[0-4]\d|[01]?\d\d?)", "192.168.300.1", False),
    # Test case for matching dates in YYYY-MM-DD format
    (r"\d{4}-\d{2}-\d{2}", "2023-06-24", True),
    (r"\d{4}-\d{2}-\d{2}", "06-24-2023", False),
    # Test case for matching phone numbers (US format)
    (r"\(\d{3}\) \d{3}-\d{4}", "(123) 456-7890", True),
    (r"\(\d{3}\) \d{3}-\d{4}", "123-456-7890", False),
    # Test case for matching credit card numbers (16 digits)
    (r"\d{4}-\d{4}-\d{4}-\d{4}", "1234-5678-9123-4567", True),
    (r"\d{4}-\d{4}-\d{4}-\d{4}", "1234-56789123-4567", False),
    # Test case for matching URLs
    (r"https?://[^\s]+", "https://example.com", True),
    (r"https?://[^\s]+", "htp://example.com", False),
    # Test case for matching a string that doesn't contain numbers
    (r"[^\d]+", "NoNumbersHere", True),
    (r"[^\d]+", "There1sANumber", False),
    # Test case for matching strings containing only uppercase letters
    (r"[A-Z]+", "UPPERCASE", True),
    (r"[A-Z]+", "LowerCase", False),
])
def test_regex_match(regex, string, expected):
    nfa = regex_to_nfa(regex)
    assert nfa_simulate(nfa, string) == expected
