/*
 * Copyright 2022 Silicon Laboratories, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

`DEFINE_ZCMP_INSTR(CM_PUSH,    ZCPP_FORMAT, STACK, RV32ZCMP)
`DEFINE_ZCMP_INSTR(CM_POP,     ZCPP_FORMAT, STACK, RV32ZCMP)
`DEFINE_ZCMP_INSTR(CM_POPRET,  ZCPP_FORMAT, STACK, RV32ZCMP)
`DEFINE_ZCMP_INSTR(CM_POPRETZ, ZCPP_FORMAT, STACK, RV32ZCMP)
`DEFINE_ZCMP_INSTR(CM_MVA01S,  ZCMV_FORMAT, STACK, RV32ZCMP)
`DEFINE_ZCMP_INSTR(CM_MVSA01,  ZCMV_FORMAT, STACK, RV32ZCMP)
