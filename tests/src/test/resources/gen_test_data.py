#!/usr/bin/env python3

# Copyright 2017 Daniel Urban
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Generates test data in the form of a JSON like this:

{
  "4bcadefd-ae75-4587-a0c1-1475feb20fc3": {
    "alpha": "9e0636fb-c553-5e62-8f08-8801ec3e0407",
    "beta": "8c0c5a0f-acd6-560a-bf12-0330cf9a47ec",
    "gamma": {
      "": "821b29a2-c730-5d1c-bdb4-37cee233ec36",
      "Alice": "e17ef951-0f2e-5a7b-8480-b00daf485477",
      ...
    },
    ...
  },
  ...
}

"""

import uuid
import random
import hashlib
import json


ABC = 'abcdefghijklmnopqrstuvwxyz'

namespaces = [
    uuid.uuid1(node=uuid._random_getnode()),
    uuid.uuid3(namespace=uuid.NAMESPACE_DNS, name='example.com'),
    uuid.uuid4(),
    uuid.uuid5(namespace=uuid.NAMESPACE_DNS, name='example.com'),
]


def names(n=10, k=8):
    '''Generate random alphabetic names

    n - the number of random names
    k - the length of the random names
    '''
    def choices(population, k):
        return [random.choice(population) for _ in range(k)]
    return (''.join(choices(population=ABC, k=k)) for _ in range(n))


def gen_uuids(ns, n=10, k=8):
    '''Generate v5 UUIDs with random names

    Return a dict of {name: UUID}

    ns - the namespace of the names
    n - the number of random names
    k - the length of the random names
    '''
    return {nm: uuid.uuid5(namespace=ns, name=nm) for nm in names(n=n, k=k)}


def mk_ns(top_ns, sub_ns=None, sub_name=None):
    '''Create a nested namespace from an UUID and a [name or UUID]'''
    if sub_ns is None:
        by = bytes(sub_name, encoding='utf-8')
    elif sub_name is None:
        by = sub_ns.bytes
    else:
        raise ValueError('both sub_ns and sub_name are specified')
    h = hashlib.sha1(top_ns.bytes + by).digest()
    return uuid.UUID(bytes=h[:16], version=5)


def gen_uuids_wns(ns, n=10, k=8):
    '''Like 'gen_uuids' but adds the namespace with empty key'''
    d = gen_uuids(ns, n, k)
    return d


def gen_test_data(ns, n=8, k=8, level=1):
    # simple names:
    d = gen_uuids_wns(ns, n=n, k=k)
    # nested namespaces:
    if level > 0:
        combined_nss = {}
        for _ in range(n // 2):
            u = uuid.uuid4()
            combined_nss[u] = mk_ns(top_ns=ns, sub_ns=u)
        for nm in names(n=(n // 2), k=k):
            combined_nss[nm] = mk_ns(top_ns=ns, sub_name=nm)
        for nested, combined in combined_nss.items():
            d[nested] = gen_test_data(ns=combined, n=n, k=k, level=(level - 1))
            d[nested][''] = combined
    return d


def stringify(d):
    if isinstance(d, dict):
        r = {}
        for k, v in d.items():
            r[str(k)] = stringify(v)
        return r
    else:
        return str(d)


if __name__ == '__main__':
    d = {ns: gen_test_data(ns) for ns in namespaces}
    j = json.dumps(stringify(d), indent='  ', sort_keys=True)
    print(j)
